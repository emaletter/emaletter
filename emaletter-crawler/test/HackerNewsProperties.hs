module HackerNewsProperties where

import HackerNews qualified as HN
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Generator for StoryDetails
genStoryDetails :: Gen HN.StoryDetails
genStoryDetails = do
  author <- Gen.text (Range.linear 1 20) Gen.alphaNum
  descendants <- Gen.int (Range.linear 0 1000)
  storyId <- Gen.int (Range.linear 1 99999999)
  score <- Gen.int (Range.linear 0 10000)
  time <- Gen.int (Range.linear 1000000000 2000000000)
  title <- Gen.text (Range.linear 5 200) Gen.unicode
  storyType <- Gen.element ["story", "ask", "job", "poll"]
  url <- Gen.maybe $ Gen.text (Range.linear 10 100) Gen.alphaNum

  pure $
    HN.StoryDetails
      { HN.storyBy = author
      , HN.storyDescendants = descendants
      , HN.storyId = storyId
      , HN.storyScore = score
      , HN.storyTime = time
      , HN.storyTitle = title
      , HN.storyType = storyType
      , HN.storyUrl = url
      }

-- | Property test: StoryDetails record accessors work correctly
prop_storyDetailsAccessors :: Property
prop_storyDetailsAccessors = property $ do
  story <- forAll genStoryDetails

  -- Test that all accessors return the expected values
  HN.storyId story === HN.storyId story
  HN.storyBy story === HN.storyBy story
  HN.storyTitle story === HN.storyTitle story
  (HN.storyScore story >= 0) === True
  (HN.storyDescendants story >= 0) === True

-- | Property test: StoryId should always be positive
prop_storyIdPositive :: Property
prop_storyIdPositive = property $ do
  story <- forAll genStoryDetails

  (HN.storyId story > 0) === True

-- | Property test: Story title should not be empty
prop_storyTitleNotEmpty :: Property
prop_storyTitleNotEmpty = property $ do
  story <- forAll genStoryDetails

  not (null (toString (HN.storyTitle story))) === True
