import           Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml = html_ "Hello Title!" body

body :: Structure
body = append_
    (append_ 
      (h1_ "Hello, world!")
      (ol_ [p_ "first", p_ "before three and after one", p_ "what?"])
    )
    (append_
      (p_ "Learning html and haskell.")
      (append_
        (p_ "Coding this with the help of lhbg-book.")
        (append_
          (p_ "Making this content into a mess.")
          (append_
            (code_ htmlCode)
            (ul_ [p_ "apple", p_ "banana", p_ "watermelon", p_ "tomato"])
          )
        )
      )
    )

htmlCode :: String
htmlCode = render (html_ "What is this?" (code_ ".&<><><>&.This is a test.&<><><>&."))
