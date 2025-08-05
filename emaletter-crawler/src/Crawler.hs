module Crawler where

import Control.Exception (try)
import Network.HTTP.Req qualified as Req

type Crawler env a = ReaderT env (ExceptT CrawlerError IO) a

newtype CrawlerError = ReqError SomeException
  deriving stock (Show)

-- | Run a crawler computation with the given environment
runCrawler :: env -> Crawler env a -> IO (Either CrawlerError a)
runCrawler env crawler = runExceptT (runReaderT crawler env)

-- | Execute an HTTP request with exception handling
tryRunReq :: (MonadIO m) => Req.Req a -> m (Either SomeException a)
tryRunReq = liftIO . try @SomeException . Req.runReq Req.defaultHttpConfig
