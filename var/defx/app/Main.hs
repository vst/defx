module Main where

import Data.Text                   as T
import Data.Time                   (Day)
import Data.Version                (showVersion)
import Defx.Commons                (Currency)
import Defx.Providers.Oxr.Programs (oxrDownloadHistoricalRates)
import Options.Applicative
       ( Parser
       , ParserInfo
       , command
       , execParser
       , fullDesc
       , header
       , help
       , helper
       , hsubparser
       , info
       , infoOption
       , long
       , metavar
       , option
       , progDesc
       , showDefault
       , strOption
       , value
       )
import Paths_defx                  (version)
import System.IO                   (stdout)

-- | Main entry-point of the application.
main :: IO ()
main = cliProgram =<< execParser cliProgramParserInfo


-- | Runs the CLI program.
cliProgram :: CliArguments -> IO ()
cliProgram (CliArguments (OxrDownloadHistorical apikey date base)) = oxrDownloadHistoricalRates apikey date base stdout


-- | Registry of commands.
data Command =
    OxrDownloadHistorical !String !Day !Currency
  deriving Show


-- | Parses program arguments.
parserProgramOptions :: Parser CliArguments
parserProgramOptions = CliArguments <$> hsubparser
  ( command "oxr-historical" (info
      (OxrDownloadHistorical
        <$> strOption (long "apikey" <> metavar "API-KEY" <> help "API key")
        <*> (read <$> strOption (long "date" <> metavar "DATE" <> help "Date to download rates for"))
        <*> (T.pack <$> strOption (long "base" <> metavar "BASE-CCY" <> value "USD" <> showDefault <> help "Base currency"))
      )
      (progDesc "Download historical FX rates for a date from OXR")
    )
  )


-- | Parsed command line arguments.
newtype CliArguments = CliArguments { cliArgumentsCommand :: Command } deriving Show


-- | Version option parser.
parserVersionOption :: Parser (a -> a)
parserVersionOption = infoOption (showVersion version) (long "version" <> help "Show version")


-- | Program parser information.
cliProgramParserInfo :: ParserInfo CliArguments
cliProgramParserInfo = info
  (helper <*> parserVersionOption <*> parserProgramOptions)
  (fullDesc <> progDesc "DEFX Program" <> header "defx")
