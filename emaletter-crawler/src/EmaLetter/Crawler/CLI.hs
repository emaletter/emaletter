module EmaLetter.Crawler.CLI (
  Command (..),
  CrawlOptions (..),
  PruneOptions (..),
  FilterOptions (..),
  PruneCriteria (..),
  parseArgs,
  runCommand,
) where

import Data.Text qualified as T
import Options.Applicative

-- | Main command data type
data Command
  = CrawlCommand CrawlOptions
  | PruneCommand PruneOptions
  | FilterCommand FilterOptions
  deriving stock (Show, Eq)

-- | Options for the crawl command
data CrawlOptions = CrawlOptions
  { crawlDelay :: !Int
  -- ^ Delay between requests in microseconds
  , crawlMaxRetries :: !Int
  -- ^ Maximum number of retry attempts
  , crawlBackoffMultiplier :: !Double
  -- ^ Exponential backoff multiplier
  }
  deriving stock (Show, Eq)

-- | Options for the prune command
data PruneOptions = PruneOptions
  { pruneCriteria :: !PruneCriteria
  , pruneDryRun :: !Bool
  -- ^ Preview mode without actual deletion
  }
  deriving stock (Show, Eq)

-- | Criteria for pruning stories
data PruneCriteria
  = -- | Keep stories newer than N days
    ByAge {days :: !Int}
  | -- | Keep only N most recent stories
    ByCount {stories :: !Int}
  deriving stock (Show, Eq)

-- | Options for the filter command
data FilterOptions = FilterOptions
  { filterKeywords :: ![Text]
  -- ^ Keywords to search for in story titles
  , filterOutput :: !FilePath
  -- ^ Output path for filtered stories
  , filterRefresh :: !Bool
  -- ^ Re-fetch story details for matched stories
  }
  deriving stock (Show, Eq)

-- | Parse command line arguments
parseArgs :: IO Command
parseArgs =
  execParser $
    info
      (commandParser <**> helper)
      ( fullDesc
          <> progDesc "Hacker News crawler with crawl, prune, and filter capabilities"
          <> header "emaletter-crawler - A robust Hacker News story crawler"
      )

-- | Main command parser
commandParser :: Parser Command
commandParser =
  subparser
    ( command "crawl" (info (CrawlCommand <$> crawlOptionsParser) (progDesc "Crawl Hacker News stories"))
        <> command "prune" (info (PruneCommand <$> pruneOptionsParser) (progDesc "Prune old stories"))
        <> command "filter" (info (FilterCommand <$> filterOptionsParser) (progDesc "Filter stories by keywords"))
    )

-- | Parser for crawl command options
crawlOptionsParser :: Parser CrawlOptions
crawlOptionsParser =
  CrawlOptions
    <$> option
      auto
      ( long "delay"
          <> short 'd'
          <> metavar "MICROSECONDS"
          <> value 1000000
          <> help "Delay between requests in microseconds (default: 1000000 = 1 second)"
      )
    <*> option
      auto
      ( long "max-retries"
          <> short 'r'
          <> metavar "INT"
          <> value 7
          <> help "Maximum retry attempts (default: 7 for 128s max backoff)"
      )
    <*> option
      auto
      ( long "backoff-multiplier"
          <> short 'b'
          <> metavar "FLOAT"
          <> value 2.0
          <> help "Exponential backoff multiplier (default: 2.0)"
      )

-- | Parser for prune command options
pruneOptionsParser :: Parser PruneOptions
pruneOptionsParser =
  PruneOptions
    <$> pruneCriteriaParser
    <*> switch
      ( long "dry-run"
          <> short 'n'
          <> help "Preview pruning actions without making changes"
      )

-- | Parser for prune criteria (mutually exclusive options)
pruneCriteriaParser :: Parser PruneCriteria
pruneCriteriaParser =
  ( ByAge
      <$> option
        auto
        ( long "days"
            <> metavar "INT"
            <> help "Keep stories newer than N days"
        )
  )
    <|> ( ByCount
            <$> option
              auto
              ( long "stories"
                  <> metavar "INT"
                  <> help "Keep only N most recent stories"
              )
        )
    <|> pure (ByAge 14) -- Default: keep stories from last 14 days

-- | Parser for filter command options
filterOptionsParser :: Parser FilterOptions
filterOptionsParser =
  FilterOptions
    <$> option
      (str >>= parseKeywords)
      ( long "keywords"
          <> short 'k'
          <> metavar "KEYWORD1,KEYWORD2,..."
          <> help "Comma-separated list of keywords to search for in story titles"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "PATH"
          <> value "feeds/hackernews.filtered.json"
          <> help "Output path for filtered story details (default: feeds/hackernews.filtered.json)"
      )
    <*> switch
      ( long "refresh"
          <> help "Re-fetch story details for matched stories to get latest data"
      )
  where
    parseKeywords :: String -> ReadM [Text]
    parseKeywords s = pure $ map T.strip $ T.splitOn "," (toText s)

-- | Execute a command (stub implementations)
runCommand :: Command -> IO ()
runCommand = \case
  CrawlCommand opts -> runCrawlCommand opts
  PruneCommand opts -> runPruneCommand opts
  FilterCommand opts -> runFilterCommand opts

-- | Stub implementation for crawl command
runCrawlCommand :: CrawlOptions -> IO ()
runCrawlCommand opts = do
  putTextLn "🚀 Running crawl command..."
  putTextLn $ "  Delay: " <> show (crawlDelay opts) <> " microseconds"
  putTextLn $ "  Max retries: " <> show (crawlMaxRetries opts)
  putTextLn $ "  Backoff multiplier: " <> show (crawlBackoffMultiplier opts)
  putTextLn "❌ TODO: Implement enhanced crawling with exponential backoff"

-- | Stub implementation for prune command
runPruneCommand :: PruneOptions -> IO ()
runPruneCommand opts = do
  putTextLn "🧹 Running prune command..."
  case pruneCriteria opts of
    ByAge d -> putTextLn $ "  Keeping stories newer than " <> show d <> " days"
    ByCount n -> putTextLn $ "  Keeping only " <> show n <> " most recent stories"
  putTextLn $ "  Dry run: " <> show (pruneDryRun opts)
  putTextLn "❌ TODO: Implement story pruning functionality"

-- | Stub implementation for filter command
runFilterCommand :: FilterOptions -> IO ()
runFilterCommand opts = do
  putTextLn "🔍 Running filter command..."
  putTextLn $ "  Keywords: " <> T.intercalate ", " (filterKeywords opts)
  putTextLn $ "  Output: " <> toText (filterOutput opts)
  putTextLn $ "  Refresh stories: " <> show (filterRefresh opts)
  putTextLn "❌ TODO: Implement keyword-based story filtering"
