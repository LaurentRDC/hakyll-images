{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Images.CompressWebp.Tests
  ( tests,
  )
where

import qualified Data.ByteString as B
import Hakyll (Identifier, Item (..))
import Hakyll.Images.Internal (Image (Image, image), ImageFormat (Jpeg), loadImage)
import Hakyll.Images.CompressWebp (compressWebpCompiler)
import Hakyll.Images.Tests.Utils (testCompilerDone)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Text.Printf (printf)

testJpg :: IO Image
testJpg = Image Jpeg <$> B.readFile "tests/data/piccolo.jpg"

fromAssertions ::
  -- | Name
  String ->
  -- | Cases
  [Assertion] ->
  -- | Result tests
  [TestTree]
fromAssertions name =
  zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

compressWebp :: Float -> Identifier -> IO Image
compressWebp quality idt = testCompilerDone idt $ do
  Item _ compressed <- loadImage >>= compressWebpCompiler quality
  return $ compressed

-- Test that the standard Image compressed to quality 25/100 is smaller
-- than the initial image
testCompressionFromImage :: Assertion
testCompressionFromImage = do
  im <- testJpg
  let initialSize = (B.length . image) im
  finalSize <- do
    testCompilerDone "piccolo.jpg" $ do
      Item _ compressed <- loadImage >>= compressWebpCompiler 25
      return $ (B.length . image) compressed

  assertBool "Image was not compressed" (initialSize > finalSize)

-- Test that specifying a WebP encoding below 0 will not fail
testWebpEncodingOutOfLowerBound :: Assertion
testWebpEncodingOutOfLowerBound = do
  compressedSize <- B.length . image <$> compressWebp (-10) "piccolo.jpg"
  expectedSize <- B.length . image <$> compressWebp 0 "piccolo.jpg"

  assertBool "Out-of-bounds WebpQuality was not handled properly" (expectedSize == compressedSize)

-- Test that specifying a WebP encoding above 100 will fail
testWebpEncodingOutOfUpperBound :: Assertion
testWebpEncodingOutOfUpperBound = do
  compressedSize <- B.length . image <$> compressWebp 150 "piccolo.jpg"
  expectedSize <- B.length . image <$> compressWebp 100 "piccolo.jpg"

  assertBool "Out-of-bounds WebpQuality was not handled properly" (expectedSize == compressedSize)

tests :: TestTree
tests =
  testGroup "Hakyll.Images.CompressWebp.Tests" $
    fromAssertions
      "compressWebp"
      [ testCompressionFromImage,
        testWebpEncodingOutOfLowerBound,
        testWebpEncodingOutOfUpperBound
      ]
