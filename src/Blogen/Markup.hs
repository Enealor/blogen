module Blogen.Markup
  ( parse,
    Document,
    Structure (..),
  )
where

import           Data.Maybe      (maybeToList)
import           Numeric.Natural (Natural)

type Document =
  [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show, Eq)

parse :: String -> Document
parse = parseLines Nothing . lines

-- | Parses a list of strings recursively with an optional context for multi-line structures.
--   The following line starts are handled:
--   - `*` -> Heading (up to six `*`)
--   - `-` -> Unordered list item
--   - `#` -> Ordered list item
--   - `>` -> Raw text item (e.g. code)
--   Other line starts are treated as text.
parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context
    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    -- This is ugly but simple.
    ('*' : '*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 2 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 3 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : '*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 4 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : '*' : '*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 5 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : '*' : '*' : '*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 6 (trim line) : parseLines Nothing rest)
    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock list) ->
          parseLines (Just (CodeBlock (list <> [line]))) rest
        _ -> maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)
    -- Paragraph case
    currentLine : rest ->
      let line = trim currentLine
       in if line == ""
            then maybe id (:) context (parseLines Nothing rest)
            else case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest -- (4)
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
