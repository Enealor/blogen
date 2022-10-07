module Blogen.Directory
  ( convertDirectory,
    buildIndex,
  )
where

import           Blogen.Convert       (convert)
import qualified Blogen.Html.Internal as Html (Html, h_, html_, link_, render,
                                               txt_)
import qualified Blogen.Markup        as Markup
import           Control.Exception    (SomeException (..), catch,
                                       displayException)
import           Control.Monad        (when)
import           Data.List            (partition)
import           Data.Traversable     (for)
import           System.Directory     (copyFile, createDirectory,
                                       doesDirectoryExist, listDirectory,
                                       removeDirectoryRecursive)
import           System.Exit          (exitFailure)
import           System.FilePath      (addExtension, takeBaseName,
                                       takeExtension, (<.>), (</>))
import           System.IO            (hPutStrLn, stderr)

-- | The relevant directory content for our application
data DirContents = DirContents
  { -- | File paths and their content
    dcFilesToProcess :: [(FilePath, String)],
    -- | Other file paths, to be copied directly
    dcFilesToCopy    :: [FilePath]
  }

-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (txtFiles, otherFiles) =
        partition ((== ".blogen") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  print txtFiles
  pure $
    DirContents
      { dcFilesToProcess = txtFilesAndContent,
        dcFilesToCopy = otherFiles
      }

-- | Try to apply an IO function on a list of values, document successes and failures
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action files = do
  for files $ \file -> do
    maybeContent <-
      catch
        (Right <$> action file)
        ( \(SomeException e) -> do
            pure $ Left (displayException e)
        )
    pure (file, maybeContent)

-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex doc_list =
  let preview =
        map
          ( \(file, doc) ->
              case doc of
                Markup.Heading 1 heading : _article ->
                  Html.h_ 3 (Html.link_ (file <.> "html") (Html.txt_ heading))
                _ -> Html.h_ 3 (Html.link_ (file <.> "html") (Html.txt_ file))
          )
          doc_list
   in Html.html_
        "index"
        ( Html.h_ 1 (Html.txt_ "Index")
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> mconcat preview
        )

-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        putStrLn "Output directory exists. Overwrite?"
        override <- confirm
        when override (removeDirectoryRecursive dir)
        pure override
      else pure True
  when create (createDirectory dir)
  pure create

confirm :: IO Bool
confirm =
  do
    putStrLn "Are you sure? (y/n)"
    answer <- getLine
    case answer of
      "y" -> pure True
      "n" -> pure False
      _ ->
        do
          putStrLn "Invalid response. use y or n"
          confirm

-- | Convert Markup file extensions and contents to Html file extensions and rendered contents.
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let markupContent = map txtToMarkup txtFiles
      index = ("index.html", buildIndex markupContent)
   in map (fmap Html.render) (index : map markupToHtml markupContent)

txtToMarkup :: (FilePath, String) -> (FilePath, Markup.Document)
txtToMarkup (filename, content) = (takeBaseName filename, Markup.parse content)

markupToHtml :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
markupToHtml (filename, content) = (addExtension filename ".html", convert filename content)

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir = foldMap (copyFile outputDir)

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir = foldMap (\(filename, contents) -> writeFile (outputDir </> filename) contents)

-- | Copy files from one directory to another, converting '.blogen' files to
--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."
