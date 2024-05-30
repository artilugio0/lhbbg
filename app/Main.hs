module Main where

import HsBlog
import OptParse

main :: IO ()
main = do
    options <- OptParse.parse
    input <-
        case options of
            ConvertSingle Stdin _ -> getContents
            ConvertSingle (InputFile path) _ -> readFile path
            _ -> undefined

    let output = case options of
            ConvertSingle _ Stdout -> putStrLn
            ConvertSingle _ (OutputFile path) -> do
              doesExist <- doesFileExist outFile
              if doesExist
                then whenIO confirm path
                else
            _ -> undefined

    output . process "un titulo" $ input

whenIO:: IO Bool -> IO () -> IO ()
whenIO = whenIOGeneric ()

confirm :: String -> IO Bool
confirm fileName = do
    putStrLn ("the file '" <> fileName <> "' already exist. Do you want to overwrite it? (y/n) [n]")
    answer <- getLine
    pure $ answer == "y"

whenIOGeneric :: a -> IO Bool -> IO a -> IO a
whenIOGeneric d condition action = do
    ok <- condition
    if ok
        then action
        else pure d
