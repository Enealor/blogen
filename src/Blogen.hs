module Blogen
  ( process
  , main
  )
  where

import qualified Blogen.Convert
import qualified Blogen.Html
import qualified Blogen.Markup
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)

process :: Blogen.Html.Title -> String -> String
process title content = Blogen.Html.render (Blogen.Convert.convert title (Blogen.Markup.parse content))

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [] ->
        do
          content <- getContents
          putStrLn (process "Title Unknown" content)
      [inputFile, outputFile] ->
        do
          content <- readFile inputFile
          outputExists <- doesFileExist outputFile
          let
            result = process inputFile content
            writeResult = writeFile outputFile result
          if outputExists then whenIO confirm writeResult else writeResult
      _ -> putStrLn "Usage: runghc example.hs [<input-file> <output-file>]"

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

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  do
    result <- cond
    if result
      then action
      else pure ()
