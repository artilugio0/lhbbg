module HsBlog
    ( process
    , convertDirectory
    , convertSingle
    )
    where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO

import HsBlog.Convert as Convert
import HsBlog.Html as Html
import HsBlog.Markup as Markup

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
    content <- hGetContents input
    hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

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
