module Main where

import Data.Set qualified as Set
import Data.Vector qualified as V
import HackerNews qualified as HN

main :: IO ()
main = do
  putTextLn "Fetching Hacker News data..."
  newestStories <- HN.fetchNewestStories
  case newestStories of
    Right storyIds -> do
      putTextLn "Saving newest story IDs..."
      HN.saveNewestStories storyIds

      -- Load existing story details and find missing ones
      existingDetails <- HN.loadStoryDetails
      let existingIds = Set.fromList $ V.toList $ fmap HN.storyId existingDetails
      let newStoryIds = V.filter (`Set.notMember` existingIds) storyIds

      putTextLn $ "Found " <> show (V.length newStoryIds) <> " new stories to fetch"

      -- Fetch missing story details with rate limiting
      if V.null newStoryIds
        then putTextLn "All stories already cached"
        else do
          newDetails <- HN.fetchAllStoryDetails newStoryIds
          HN.saveStoryDetails newDetails
          putTextLn $ "Fetched and saved " <> show (V.length newDetails) <> " story details"
    Left err ->
      putTextLn $ "Failed to fetch Hacker News data: " <> show err
