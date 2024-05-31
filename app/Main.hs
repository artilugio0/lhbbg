module Main where

import HsBlog
import OptParse
import System.Directory (doesFileExist)
import System.IO
import System.Exit (exitFailure)

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.convertDirectory input output

    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode

      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm file
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure

      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

confirm :: String -> IO Bool
confirm fileName = do
    putStrLn ("the file '" <> fileName <> "' already exist. Do you want to overwrite it? (y/n) [n]")
    answer <- getLine
    pure $ answer == "y"
