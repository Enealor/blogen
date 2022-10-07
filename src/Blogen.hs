module Blogen
  ( process,
    convertSingle,
    convertDirectory,
  )
where

import qualified Blogen.Convert
import qualified Blogen.Directory
import qualified Blogen.Html
import qualified Blogen.Markup
import           System.IO        (Handle, hGetContents, hPutStrLn)

process :: Blogen.Html.Title -> String -> String
process title content = Blogen.Html.render (Blogen.Convert.convert title (Blogen.Markup.parse content))

convertSingle :: Blogen.Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = Blogen.Directory.convertDirectory
