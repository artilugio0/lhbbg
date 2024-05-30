module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html

convertStructure :: Markup.Structure -> Html.Structure
convertStructure (Markup.Heading n text) = Html.h_ n text
convertStructure (Markup.Paragraph text) = Html.p_ text
convertStructure (Markup.UnorderedList items) =
    Html.ul_ . map Html.p_ $ items
convertStructure (Markup.OrderedList items) =
    Html.ol_ . map Html.p_ $ items
convertStructure (Markup.CodeBlock items) =
    Html.code_ . unlines $ items

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
