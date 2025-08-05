module Main where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import HackerNewsProperties qualified as HProps
import HackerNewsSpec qualified as HSpec

main :: IO ()
main = do
  hspecTests <- testSpec "HackerNews Hspec Tests" HSpec.spec_hackerNewsBasics

  defaultMain $
    testGroup
      "All Tests"
      [ hspecTests
      , testGroup
          "Hedgehog Properties"
          [ testProperty "StoryDetails accessors work correctly" HProps.prop_storyDetailsAccessors
          , testProperty "StoryId is always positive" HProps.prop_storyIdPositive
          , testProperty "Story title is not empty" HProps.prop_storyTitleNotEmpty
          ]
      ]
