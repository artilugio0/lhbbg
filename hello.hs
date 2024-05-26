import Html

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
