module HackerNews where

import Autodocodec (HasCodec (..), object, optionalField, parseJSONViaCodec, requiredField, toJSONViaCodec, (.=))
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Network.HTTP.Req qualified as Req
import Relude.Extra.Bifunctor (bimapF)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

type StoryId = Int

-- | Hacker News story details structure
data StoryDetails = StoryDetails
  { storyBy :: !Text
  , storyDescendants :: !Int
  , storyId :: !StoryId
  , storyScore :: !Int
  , storyTime :: !Int
  , storyTitle :: !Text
  , storyType :: !Text
  , storyUrl :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance HasCodec StoryDetails where
  codec =
    object "StoryDetails" $
      StoryDetails
        <$> requiredField "by" "Story author" .= storyBy
        <*> requiredField "descendants" "Number of comments" .= storyDescendants
        <*> requiredField "id" "Story ID" .= storyId
        <*> requiredField "score" "Story score" .= storyScore
        <*> requiredField "time" "Story timestamp" .= storyTime
        <*> requiredField "title" "Story title" .= storyTitle
        <*> requiredField "type" "Story type" .= storyType
        <*> optionalField "url" "Story URL" .= storyUrl

instance FromJSON StoryDetails where
  parseJSON = parseJSONViaCodec

instance ToJSON StoryDetails where
  toJSON = toJSONViaCodec

newtype CrawlerError = ReqError SomeException
  deriving stock (Show)

-- | API URL for fetching newest story IDs from Hacker News
newStoriesUrl :: Req.Url Req.Https
newStoriesUrl = Req.https "hacker-news.firebaseio.com" Req./: "v0" Req./: "newstories.json"

-- | Local file path for storing newest story IDs
newStoriesFilePath :: FilePath
newStoriesFilePath = "feeds/hackernews.newstories.json"

-- | HTTP request configuration for fetching newest story IDs
newStoriesReq :: Req.Req (Req.JsonResponse (Vector StoryId))
newStoriesReq =
  Req.req
    Req.GET
    newStoriesUrl
    Req.NoReqBody
    Req.jsonResponse
    mempty

-- | Generate API URL for fetching details of a specific story
storyDetailsUrl :: StoryId -> Req.Url Req.Https
storyDetailsUrl storyId' =
  Req.https "hacker-news.firebaseio.com" Req./: "v0" Req./: "item" Req./: show storyId' <> ".json"

-- | Local file path for storing story details
storyDetailsFilePath :: FilePath
storyDetailsFilePath = "feeds/hackernews.storydetails.json"

-- | HTTP request configuration for fetching story details by ID
storyDetailsReq :: StoryId -> Req.Req (Req.JsonResponse StoryDetails)
storyDetailsReq storyId' =
  Req.req
    Req.GET
    (storyDetailsUrl storyId')
    Req.NoReqBody
    Req.jsonResponse
    mempty

-- | Execute an HTTP request with exception handling
tryRunReq :: forall {a}. Req.Req a -> IO (Either SomeException a)
tryRunReq = try @SomeException . Req.runReq Req.defaultHttpConfig

-- | Fetch the list of newest story IDs from Hacker News
fetchNewestStories :: (MonadIO m) => m (Either CrawlerError (Vector StoryId))
fetchNewestStories =
  liftIO $ bimapF ReqError Req.responseBody (tryRunReq newStoriesReq)

-- | Save the list of newest story IDs to file
saveNewestStories :: (MonadIO m) => Vector StoryId -> m ()
saveNewestStories storyIds = do
  liftIO $ createDirectoryIfMissing True (takeDirectory newStoriesFilePath)
  liftIO $ writeFileLBS newStoriesFilePath (encode storyIds)

-- | Fetch detailed information for a specific story ID
fetchStoryDetails :: (MonadIO m) => StoryId -> m (Either CrawlerError StoryDetails)
fetchStoryDetails storyId' =
  liftIO $ bimapF ReqError Req.responseBody (tryRunReq (storyDetailsReq storyId'))

-- | Fetch story details for multiple IDs with rate limiting (1 second between requests)
fetchAllStoryDetails :: (MonadIO m) => Vector StoryId -> m (Vector StoryDetails)
fetchAllStoryDetails storyIds = do
  results <- liftIO $ V.imapM fetchWithDelay storyIds
  pure $ V.mapMaybe rightToMaybe results
  where
    fetchWithDelay :: Int -> StoryId -> IO (Either CrawlerError StoryDetails)
    fetchWithDelay idx storyId' = do
      when (idx > 0) $ threadDelay 1_000_000 -- 1 second delay
      result <- bimapF ReqError Req.responseBody (tryRunReq (storyDetailsReq storyId'))
      case result of
        Right details -> putTextLn $ "Fetched: " <> storyTitle details
        Left err -> putTextLn $ "Failed to fetch story " <> show storyId' <> ": " <> show err
      pure result

-- | Load existing story details from disk
loadStoryDetails :: (MonadIO m) => m (Vector StoryDetails)
loadStoryDetails = do
  exists <- liftIO $ doesFileExist storyDetailsFilePath
  if not exists
    then pure mempty
    else do
      content <- liftIO $ readFileLBS storyDetailsFilePath
      liftIO $ evaluateNF (maybeToMonoid $ decode content)

-- | Save story details to disk, adding new details to existing ones
saveStoryDetails :: (MonadIO m) => Vector StoryDetails -> m ()
saveStoryDetails newStoryDetails = do
  liftIO $ createDirectoryIfMissing True (takeDirectory storyDetailsFilePath)
  existingDetails <- loadStoryDetails
  let combinedDetails = newStoryDetails <> existingDetails
  liftIO $ writeFileLBS storyDetailsFilePath (encode combinedDetails)
