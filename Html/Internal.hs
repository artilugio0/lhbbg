module Html.Internal where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

getStructureString :: Structure -> String
getStructureString s =
    case s of
        Structure str -> str

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
    (Html . el "html" . getStructureString)
        (append_
            (head_ (getStructureString (title_  title)))
            (body_ content))

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

body_ :: String -> Structure
body_ = Structure . el "body"

head_ :: String -> Structure
head_ = Structure . el "head"

title_ :: String -> Structure
title_ = Structure . el "title" . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) =
    Structure (s1 <> s2)

render :: Html -> String
render (Html str) = str

escape :: String -> String
escape =
    let
        escapeChar c =
            case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
    in
        concat . map escapeChar

ul_ :: [Structure] -> Structure
ul_ = list_ "ul"

ol_ :: [Structure] -> Structure
ol_ = list_ "ol"

list_ :: String -> [Structure] -> Structure
list_ listType =
    Structure . el listType
    . concat
    . map (\s -> "<li>" <> s <> "</li>")
    . map getStructureString
