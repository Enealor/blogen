# Revision history for blogen

## 0.1.3.0 -- 2022-10-06

* Remembered to push commits and update this file.
* Added `Blogen.Directory` module to convert directories of files.
* Added `test` folder.
* Fixed bug in processing of code markup content. Previously, it would strip whitespace. Whitespace is now preserved.


## 0.1.2.0 -- 2022-09-15

* Added `Blogen.Html.Content` type to replace the use of `String`.
* Added index building.
* Added the ability to convert up to 6 heading levels (`*` through `******`) and examples.


## 0.1.1.0 -- 2022-09-12

* Restructured to use Cabal build system. All modules are now under the `Blogen` namespace.
* Added Monoid and Semigroup instance to `Blogen.Html.Structure` type.
* Added `Blogen.Markup` module to parse input files.
* Changed the module entry point to process command line arguments. Previously, all content was in the script.
* Added `Blogen.Convert` module to convert `Blogen.Markup` documents to `Blogen.Html`.
* Moved command line processing to executable. Added `OptParse` module for parsing.


## 0.1.0.0 -- 2022-09-11

* First version. Released on an unsuspecting world.
* Created initial HTML DSL, and refactored into module. (Chapter 3.4)
* Added `HTML` module. (Chapter 3.6)
* Added `HTML.Internal` module. (End of chapter 3)
