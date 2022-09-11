import           Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml = html_
  "<Hello Title!>"
  (append_
    (h1_ "<tag>&Hello, world!</tag>")
    (append_
      (p_ "\"Learning html and haskell.\"")
      (append_
        (p_ "Coding this with the help of lhbg-book.")
        (p_ "\'Making this content into a mess.\'")
      )
    )
  )
