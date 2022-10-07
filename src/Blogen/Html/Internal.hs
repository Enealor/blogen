module Blogen.Html.Internal where

import           GHC.Natural (Natural)

-- * Types

-- | Html type representing a complete html document
newtype Html = Html String

-- | Html type representing structures such as headings and paragraphs to
--   be embedded into an html document.
newtype Structure = Structure String

instance Semigroup Structure where
  Structure a <> Structure b = Structure (a <> b)

instance Monoid Structure where
  mempty = empty_

-- | Html type representing the content that can be passed to a structure.
newtype Content = Content String

-- | Html type alias for Title.
type Title = String

-- * EDSL

-- | Converts title and an html structure into an html document.
--   The html structure is used as the body of the document.
html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

-- | Wraps content in a p tag.
p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

-- | Wraps content in a h1 tag.
h1_ :: Content -> Structure
h1_ = Structure . el "h1" . getContentString

-- | Wraps content in a section header tag.
h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

-- | Wraps content in a pre tag.
code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- | Converts a list of structures into an unordered list structure.
ul_ :: [Structure] -> Structure
ul_ structureList = Structure (el "ul" (getStructureString (toListStructure structureList)))

-- | Converts a list of structures into an ordered list structure.
ol_ :: [Structure] -> Structure
ol_ structureList = Structure (el "ol" (getStructureString (toListStructure structureList)))

-- | Turns a string into Html content
txt_ :: String -> Content
txt_ = Content . escape

b_ :: Content -> Content
b_ (Content content) = Content (el "b" content)

i_ :: Content -> Content
i_ (Content content) = Content (el "i" content)

img_ :: FilePath -> Content -> Content
img_ source altText = Content ("<img src=\"" <> escape source <> "\" alt=\"" <> getContentString altText <> "\">")

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

empty_ :: Structure
empty_ = Structure ""

concatStructure :: [Structure] -> Structure
concatStructure = mconcat

-- -- | Combines structure a and structure b into a single structure.
-- --   In effect, this is string concatination.
-- append_ :: Structure -> Structure -> Structure
-- append_ (Structure a) (Structure b) = Structure (a <> b)

-- * Render

-- | Returns html content as a string.
render :: Html -> String
render (Html a) = a

-- * Utilities

-- | Gets string content from a structure.
getStructureString :: Structure -> String
getStructureString (Structure a) = a

-- | Gets string value from a Content.
getContentString :: Content -> String
getContentString (Content a) = a

-- | Wraps content in the provided tag.
--   e.g. el "foo" "bar" = "<foo>bar</foo>"
el :: String -> String -> String
el tagName content = "<" <> tagName <> ">" <> content <> "</" <> tagName <> ">"

-- | Wraps content in the provided tag with attributes.
--   e.g. elAttr "foo" "href=\"..\"" "bar" = "<foo href=\"..\">bar</foo>"
elAttr :: String -> String -> String -> String
elAttr tagName tagAttrs content = "<" <> tagName <> " " <> tagAttrs <> ">" <> content <> "</" <> tagName <> ">"

-- | Replaces html specific characters with "safe" versions.
escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<'  -> "&lt;"
          '>'  -> "&gt;"
          '&'  -> "&amp;"
          '"'  -> "&quot;"
          '\'' -> "&#39;"
          _    -> [c]
   in concatMap escapeChar

-- | Wraps a structure for use in a list.
listItemWrap :: Structure -> Structure
listItemWrap (Structure content) = Structure (el "li" content)

-- | Converts a list of structures into a single structure. Each structure has the <li> tag added.
toListStructure :: [Structure] -> Structure
toListStructure = Structure . concatMap (el "li" . getStructureString)

-- -- Initial recursive approach
-- toListStructure (i:is) = append_ (listItemWrap i) (toListStructure is)
-- toListStructure [] = Structure ""
