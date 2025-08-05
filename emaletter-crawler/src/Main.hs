module Main where

import Data.Set qualified as Set
import Data.Vector qualified as V
import HackerNews qualified as HN
import Relude.Extra.Bifunctor (firstF)

main :: IO ()
main = do
  putTextLn "Fetching Hacker News data..."
  result <- runExceptT processStories
  case result of
    Left err -> putTextLn $ "Failed to fetch Hacker News data: " <> show err
    Right () -> pass

processStories :: (MonadIO m) => ExceptT String m ()
processStories = do
  storyIds <- ExceptT $ firstF show HN.fetchNewestStories
  liftIO $ putTextLn "Saving newest story IDs..."
  liftIO $ HN.saveNewestStories storyIds

  -- Load existing story details and find missing ones
  existingDetails <- liftIO HN.loadStoryDetails
  let existingIds = Set.fromList $ V.toList $ fmap HN.storyId existingDetails
  let newStoryIds = V.filter (`Set.notMember` existingIds) storyIds

  liftIO $ putTextLn $ "Found " <> show (V.length newStoryIds) <> " new stories to fetch"

  -- Fetch missing story details with rate limiting
  if V.null newStoryIds
    then liftIO $ putTextLn "All stories already cached"
    else do
      newDetails <- liftIO $ HN.fetchAllStoryDetails newStoryIds
      liftIO $ HN.saveStoryDetails newDetails
      liftIO $ putTextLn $ "Fetched and saved " <> show (V.length newDetails) <> " story details"
