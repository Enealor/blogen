{-# LANGUAGE QuasiQuotes #-}
module MarkupParsingSpec where
import           Blogen.Markup     (Structure (CodeBlock, Heading, OrderedList, Paragraph, UnorderedList),
                                    parse)
import           Test.Hspec
import           Text.RawString.QQ

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple
    -- multiline

simple :: Spec
simple = do
  describe "Simple tests" $ do
    it "empty" $
      shouldBe
        (parse "")
        []

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "heading 2" $
      shouldBe
        (parse "** Heading 2")
        [Heading 2 "Heading 2"]

    it "heading 3" $
      shouldBe
        (parse "*** Heading 3")
        [Heading 3 "Heading 3"]

    it "heading 4" $
      shouldBe
        (parse "**** Heading 4")
        [Heading 4 "Heading 4"]

    it "heading 5" $
      shouldBe
        (parse "***** Heading 5")
        [Heading 5 "Heading 5"]

    it "heading 6" $
      shouldBe
        (parse "****** Heading 6")
        [Heading 6 "Heading 6"]

    it "unordered list" $
      shouldBe
        (parse "- 1-i")
        [UnorderedList ["1-i"]]

    it "ordored list" $
      shouldBe
        (parse "# a")
        [OrderedList ["a"]]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world!\"")
        [CodeBlock ["main = putStrLn \"hello world!\""]]

    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

example1 :: String
example1 = [r|
Hello, world!

|]

example2 :: String
example2 = [r|
* Welcome

To this tutorial about Haskell.
|]

example3 :: String
example3 = [r|
Remember that multiple lines with no separation
are grouped together to a single paragraph
but list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]

example4 :: String
example4 = [r|
* Compiling programs with ghc

Running ghc invokes the Glasgow Haskell Compiler (GHC),
and can be used to compile Haskell modules and programs into native
executables and libraries.

Create a new Haskell source file named hello.hs, and write
the following code in it:

> main = putStrLn "Hello, Haskell!"

Now, we can compile the program by invoking ghc with the file name:

> ➜ ghc hello.hs
> [1 of 1] Compiling Main             ( hello.hs, hello.o )
> Linking hello ...

GHC created the following files:

- hello.hi - Haskell interface file
- hello.o - Object file, the output of the compiler before linking
- hello (or hello.exe on Microsoft Windows) - A native runnable executable.

GHC will produce an executable when the source file satisfies both conditions:

# Defines the main function in the source file
# Defines the module name to be Main, or does not have a module declaration

Otherwise, it will only produce the .o and .hi files.
|]

multiline :: Spec
multiline = do
  describe "multiline parsing" $ do
    it "example1" $
      shouldBe
        (parse example1)
        [Paragraph "Hello, world!"]
    it "example2" $
      shouldBe
        (parse example2)
        [
          Heading 1 "Welcome",
          Paragraph "To this tutorial about Haskell."
        ]
    it "example3" $
      shouldBe
        (parse example3)
        [
          Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate.",
          OrderedList ["Item 1 of a list", "Item 2 of the same list"]
        ]
    it "example4" $
      shouldBe
        (parse example4)
        [
          Heading 1 "Compiling programs with ghc",
          Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries.",
          Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:",
          CodeBlock ["main = putStrLn \"Hello, Haskell!\""],
          Paragraph "Now, we can compile the program by invoking ghc with the file name:",
          CodeBlock ["➜ ghc hello.hs", "[1 of 1] Compiling Main             ( hello.hs, hello.o )", "Linking hello ..."],
          Paragraph "GHC created the following files:",
          UnorderedList ["hello.hi - Haskell interface file", "hello.o - Object file, the output of the compiler before linking", "hello (or hello.exe on Microsoft Windows) - A native runnable executable."],
          Paragraph "GHC will produce an executable when the source file satisfies both conditions:",
          OrderedList ["Defines the main function in the source file", "Defines the module name to be Main, or does not have a module declaration"],
          Paragraph "Otherwise, it will only produce the .o and .hi files."
        ]
