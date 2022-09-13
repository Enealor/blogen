module OptParse
  ( Options(..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  )
  where

import           Data.Maybe          (fromMaybe)
import           Options.Applicative (Parser, ParserInfo, command, execParser,
                                      fullDesc, header, help, helper, info,
                                      long, metavar, optional, progDesc, short,
                                      strOption, subparser, switch)

data Options
  = ConvertSingle SingleInput SingleOutput Bool
  | ConvertDir FilePath FilePath Bool
  deriving Show

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

-- Single file parser
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser -- fmap and <$> are the same
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

-- dir parser

pInputDir :: Parser FilePath
pInputDir =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "DIR"
          <> help "Input directory"
        )

pOutputDir :: Parser FilePath
pOutputDir =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "DIR"
          <> help "Output directory"
        )

pReplace :: Parser Bool
pReplace =
      switch
        ( long "replace"
          <> help "Replace output without prompting."
        )

pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput <*> pReplace

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir <*> pReplace

pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        ( helper <*> pConvertSingle)
        ( progDesc "Convert a single markup source to html"))
      <> command
      "convert-dir"
      ( info
        ( helper <*> pConvertDir)
        ( progDesc "Convert a directory of markup source files to html")))

opts :: ParserInfo Options
opts =
  info (helper <*> pOptions)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

parse :: IO Options
parse = execParser opts
