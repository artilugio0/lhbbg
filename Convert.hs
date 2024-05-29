module Convert where

import qualified Markup
import qualified Html

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
