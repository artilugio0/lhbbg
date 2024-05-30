module HsBlog
    ( main
    , process
    )
    where

import Data.Maybe
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Directory (doesFileExist)

import HsBlog.Convert as Convert
import HsBlog.Html as Html
import HsBlog.Markup as Markup

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= putStrLn . process "un titulo"

        [inFile, outFile] -> do
           doesExist <- doesFileExist outFile
           let
               convertFile =
                   (readFile inFile >>=
                   (pure . process "un titulo") >>=
                   (writeFile outFile))

           if doesExist
               then whenIO (confirm outFile) convertFile
               else convertFile

        otherwise -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

convertFile :: String -> String -> IO ()
convertFile inFile outFile = do
    content <- readFile inFile
    let htmlContent = process "un titulo" content
    writeFile outFile htmlContent

whenIO:: IO Bool -> IO () -> IO ()
whenIO = whenIOGeneric ()

whenIOGeneric :: a -> IO Bool -> IO a -> IO a
whenIOGeneric d condition action = do
    ok <- condition
    if ok
        then action
        else pure d

confirm :: String -> IO Bool
confirm fileName = do
    putStrLn ("the file '" <> fileName <> "' already exist. Do you want to overwrite it? (y/n) [n]")
    answer <- getLine
    pure $ answer == "y"

process :: String -> String -> String
process title = render . Convert.convert title . Markup.parse
