main :: IO ()
main = putStrLn (render_ myhtml)

-- | Html type representing full html document
newtype Html = Html String

-- | Html type representing structures such as headings and paragraphs
newtype Structure = Structure String

-- | Html type representing the 
type Title = String

myhtml :: Html
myhtml = html_ "Hello Title!" (append_ (h1_ "Hello, world!") (p_ "Learning html and haskell."))

html_ :: Title -> Structure -> Html
-- | Converts title and an html structure into an html document.
--   The html structure is used as the body of the document.
html_ title content = 
  Html
    (el "html"
      ( el "head" (el "title" title)
      <> el "body" (getStructureString content)
      )
    )

-- body_ :: String -> Structure
-- -- | Wraps content in <body> tag.
-- body_ = Structure . el "body"

-- head_ :: String -> Structure
-- -- | Wraps content in <head> tag.
-- head_ = Structure . el "head"

-- title_ :: String -> Structure
-- -- | Wraps content in <title> tag.
-- title_ = Structure . el "title"

p_ :: String -> Structure
-- | Wraps content in <p> tag.
p_ = Structure . el "p"

h1_ :: String -> Structure
-- | Wraps content in <h1> tag.
h1_ = Structure . el "h1"

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

render_ :: Html -> String
render_ (Html a) = a

getStructureString :: Structure -> String
getStructureString (Structure a) = a

-- makeHtml :: Title -> Structure -> Html
-- makeHtml title content = html_ (append_ (head_ (title_ title)) (body_ content))

el :: String -> String -> String
-- | Wraps content in the provided tag.
--   e.g. tag "foo" "bar" = "<foo>bar</foo>"
el tagName content = "<" <> tagName <> ">" <> content <> "</" <> tagName <> ">"
