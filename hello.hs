import Html
import Markup

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
