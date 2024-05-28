import Html
import Markup
import Data.Maybe
import Data.Word (Word8)

main :: IO()
main = (putStrLn . render) myhtml

myhtml :: Html
myhtml =
    (html_
        "Aprendiendo Haskell <3"
        (append_ 
            (h1_ "Aprendiendo Haskell en Twitch")
            (append_
                (p_ "En este stream estamos aprendiendo los conceptos basicos de Haskell")
                (append_
                    (code_ "esto es codigo")
                    (ol_
                        [ p_ "item 1"
                        , p_ "item 2"
                        , p_ "item 3"
                        , p_ "item 4"
                        , p_ "item 5"
                        ])))))

helloWorld :: Document
helloWorld = [ Paragraph "Hello, world!" ]

headingParagraph :: Document
headingParagraph =
    [ Heading 1 "Welcome"
    , Paragraph "To this tutorial about Haskell."
    ]

paragraphOrderedList :: Document
paragraphOrderedList =
    [ Paragraph "Remember that multiple lines with no separation are grouped together into a single paragraph but list items remain separate."
    , OrderedList
        [ "Item 1 of a list"
        , "Item 2 of the same list"
        ]
    ]

documentoCompleto :: Document
documentoCompleto =
    [ Heading 1 "Compiling programs with ghc"
    , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
    , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
    , CodeBlock
        [ "main = putStrLn \"Hello, Haskell!\""
        ]
    , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
    , CodeBlock
        [ "➜ ghc hello.hs"
        , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
        , "Linking hello ..."
        ]
    , Paragraph "GHC created the following files:"
    , UnorderedList
        [ "hello.hi - Haskell interface file"
        , "hello.o - Object file, the output of the compiler before linking"
        , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
        ]
    , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
    , OrderedList
        [ "Defines the main function in the source file"
        , "Defines the module name to be Main or does not have a module declaration"
        ]
    , Paragraph "Otherwise, it will only produce the .o and .hi files."
    ]

newDoc :: Document
newDoc =
    [ Heading 1 "Esto es un titulo"
    , Paragraph "esto es un parrafo"
    ]


replicate2 :: Int -> a -> [a]
replicate2 n elem =
    if n <= 0 
        then []
        else elem : (replicate2 (n-1) elem)

even2 :: Int -> Bool
even2 n =
    if n == 0
        then True
        else if n > 0
            then odd2 (n-1)
            else odd2 (n+1)

odd2 :: Int -> Bool
odd2 n =
    if n == 0
        then False
        else if n > 0
            then even2 (n-1)
            else even2 (n+1)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

isBright :: AnsiColor -> Bool
isBright (AnsiColor Bright _) = True
isBright _ = False

data Color
  = RGB Word8 Word8 Word8 deriving Show

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu (AnsiColor Dark Black) = RGB 1 1 1
ansiToUbuntu (AnsiColor Dark Red) = RGB 222 56 43
ansiToUbuntu (AnsiColor Dark Green) = RGB 57 181 74
ansiToUbuntu (AnsiColor Dark Yellow) = RGB 255 199 6
ansiToUbuntu (AnsiColor Dark Blue) = RGB 0 111 184
ansiToUbuntu (AnsiColor Dark Magenta) = RGB 118 38 113
ansiToUbuntu (AnsiColor Dark Cyan) = RGB 44 181 233
ansiToUbuntu (AnsiColor Dark White) = RGB 204 204 204
ansiToUbuntu (AnsiColor Bright Black) = RGB 128 128 128
ansiToUbuntu (AnsiColor Bright Red) = RGB 255 0 0
ansiToUbuntu (AnsiColor Bright Green) = RGB 0 255 0
ansiToUbuntu (AnsiColor Bright Yellow) = RGB 255 255 0
ansiToUbuntu (AnsiColor Bright Blue) = RGB 0 0 255
ansiToUbuntu (AnsiColor Bright Magenta) = RGB 255 0 255
ansiToUbuntu (AnsiColor Bright Cyan) = RGB 0 255 255
ansiToUbuntu (AnsiColor Bright White) = RGB 255 255 255

isEmpty :: [a] -> Bool
isEmpty l =
    case listToMaybe l of
        Nothing -> True
        Just _ -> False

isEmpty2 :: [a] -> Bool
isEmpty2 [] = True
isEmpty2 _ = False
