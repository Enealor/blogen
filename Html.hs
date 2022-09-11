module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where

-- * Types

-- | Html type representing a complete html document
newtype Html = Html String

-- | Html type representing structures such as headings and paragraphs to
--   be embedded into an html document.
newtype Structure = Structure String

-- | Html type alias for Title.
type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
-- | Converts title and an html structure into an html document.
--   The html structure is used as the body of the document.
html_ title content =
  Html
    (el "html"
      ( el "head" (el "title" (escape title))
      <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
-- | Wraps content in <p> tag.
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
-- | Wraps content in <h1> tag.
h1_ = Structure . el "h1" . escape

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

-- * Render

render :: Html -> String
render (Html a) = a

-- * Utilities

getStructureString :: Structure -> String
getStructureString (Structure a) = a

el :: String -> String -> String
-- | Wraps content in the provided tag.
--   e.g. tag "foo" "bar" = "<foo>bar</foo>"
el tagName content = "<" <> tagName <> ">" <> content <> "</" <> tagName <> ">"

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<'  -> "&lt;"
        '>'  -> "&gt;"
        '&'  -> "&amp;"
        '"'  -> "&quot;"
        '\'' -> "&#39;"
        _    -> [c]
  in
    concatMap escapeChar
