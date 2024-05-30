module OptParse
    ( Options(..)
    , SingleInput(..)
    , SingleOutput(..)
    , parse
    )
    where

import Options.Applicative
import Data.Maybe (fromMaybe)

data Options
    = ConvertSingle SingleInput SingleOutput
    | ConvertDir FilePath FilePath
    deriving Show

data SingleInput
    = Stdin
    | InputFile FilePath
    deriving Show

data SingleOutput
    = Stdout
    | OutputFile FilePath
    deriving Show

parse :: IO Options
parse = execParser opts

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
    where
        parser =
            strOption
                (  long "input"
                <> short 'i'
                <> metavar "FILE"
                <> help "Input File"
                )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
    where
        parser =
            strOption
                (  long "output"
                <> short 'o'
                <> metavar "FILE"
                <> help "Output File"
                )

pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

pInputDir :: Parser FilePath
pInputDir =
    strOption
        (  long "input"
        <> short 'i'
        <> metavar "DIR"
        <> help "Input Directory"
        )

pOutputDir :: Parser FilePath
pOutputDir =
    strOption
        (  long "output"
        <> short 'o'
        <> metavar "DIR"
        <> help "Output Directory"
        )

pConvertSingle :: Parser Options
pConvertSingle =
    ConvertSingle
        <$> pSingleInput
        <*> pSingleOutput

pConvertDir :: Parser Options
pConvertDir =
    ConvertDir
        <$> pInputDir
        <*> pOutputDir

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
    info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo =
    info
        (helper <*> pConvertDir)
        (progDesc "Convert all markup source files in a directory to html")

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand = command "convert" pConvertSingleInfo

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand = command "convert-dir" pConvertDirInfo

pConvertSingleCommandParser :: Parser Options
pConvertSingleCommandParser = subparser pConvertSingleCommand

pConvertDirCommandParser :: Parser Options
pConvertDirCommandParser = subparser pConvertDirCommand

mypOption :: Parser Options
mypOption = pConvertDirCommandParser <|> pConvertSingleCommandParser

pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to html")
      )
    )

opts :: ParserInfo Options
opts =
    info (helper <*> pOptions)
        ( fullDesc
            <> header "hs-blog-gen - a static blog generator"
            <> progDesc "Convert markup files or directories to html"
        )
