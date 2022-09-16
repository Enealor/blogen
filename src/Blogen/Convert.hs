module Blogen.Convert
  ( convert
  , buildIndex
  , convertStructure
  )
  where

import qualified Blogen.Html   as Html
import qualified Blogen.Markup as Markup

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex doc_list =
  let
    preview =
      map
        (\(file, doc) ->
          case doc of
            Markup.Heading 1 heading : _article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ heading))
            _ -> Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
      doc_list
  in
    Html.html_
      "index"
      ( Html.h_ 1 (Html.txt_ "Index")
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat preview
      )

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt

    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list

    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
