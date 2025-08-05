module HackerNewsSpec where

import HackerNews qualified as HN
import Test.Hspec

-- | Dummy Hspec test to verify test infrastructure
spec_hackerNewsBasics :: Spec
spec_hackerNewsBasics = describe "HackerNews module" $ do
  it "creates StoryDetails with correct fields" $ do
    let story =
          HN.StoryDetails
            { HN.storyBy = "testuser"
            , HN.storyDescendants = 42
            , HN.storyId = 12345
            , HN.storyScore = 100
            , HN.storyTime = 1640995200
            , HN.storyTitle = "Test Story Title"
            , HN.storyType = "story"
            , HN.storyUrl = Just "https://example.com"
            }

    HN.storyBy story `shouldBe` "testuser"
    HN.storyId story `shouldBe` 12345
    HN.storyTitle story `shouldBe` "Test Story Title"
    HN.storyUrl story `shouldBe` Just "https://example.com"

  it "handles story URLs as optional" $ do
    let storyWithoutUrl =
          HN.StoryDetails
            { HN.storyBy = "author"
            , HN.storyDescendants = 0
            , HN.storyId = 67890
            , HN.storyScore = 1
            , HN.storyTime = 1640995200
            , HN.storyTitle = "Ask HN: Some question"
            , HN.storyType = "story"
            , HN.storyUrl = Nothing
            }

    HN.storyUrl storyWithoutUrl `shouldBe` Nothing
    toString (HN.storyTitle storyWithoutUrl) `shouldContain` "Ask HN"
