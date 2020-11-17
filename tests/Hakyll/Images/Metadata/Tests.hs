{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Hakyll.Images.Metadata.Tests
  ( tests,
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Hakyll.Images
import Hakyll.Images.Common
import qualified Hakyll.Images.Metadata as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Text.Printf (printf)

fromAssertions ::
  -- | Name
  String ->
  -- | Cases
  [Assertion] ->
  -- | Result tests
  [TestTree]
fromAssertions name =
  zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

testJpg :: IO Image
testJpg = Image Jpeg <$> (B.readFile "tests/data/piccolo.jpg")

testMetadata :: Assertion
testMetadata = do
  im <- testJpg
  let meta = metadata im
      height = fromMaybe 0 (M.lookup M.Height meta)
      width = fromMaybe 0 (M.lookup M.Width meta)

  -- The following values of (width, height) = (1170, 647) are
  -- specific to the "piccolo.jpg" image
  assertBool "Metadata was not decoded properly" (height == 647)
  assertBool "Metadata was not decoded properly" (width == 1170)

tests :: TestTree
tests =
  testGroup "Hakyll.Images.Metadata.Tests" $
    concat
      [ fromAssertions "metadata" [testMetadata]
      ]
