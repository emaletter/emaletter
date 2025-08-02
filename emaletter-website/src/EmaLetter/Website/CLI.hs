module EmaLetter.Website.CLI (
  CliArgs (..),
  parseCliArgs,
) where

import Ema.CLI qualified
import Options.Applicative

data CliArgs = CliArgs
  { cliArgsBaseUrl :: Text
  , cliArgsEmaCli :: Ema.CLI.Cli
  }
  deriving stock (Eq, Show)

parseCliArgs :: IO CliArgs
parseCliArgs =
  execParser $ parserInfo cliParser
  where
    cliParser :: Parser CliArgs
    cliParser =
      CliArgs
        <$> option str (long "base-url" <> metavar "BASE_URL" <> help "Base URL to use in <base>" <> value "/")
        <*> Ema.CLI.cliParser
    parserInfo :: Parser a -> ParserInfo a
    parserInfo p =
      info
        (versionOption <*> p <**> helper)
        ( fullDesc
            <> progDesc "emaletter-website: TODO"
            <> header "emaletter-website"
        )
      where
        versionOption = infoOption "0.1" (long "version" <> help "Show version")
