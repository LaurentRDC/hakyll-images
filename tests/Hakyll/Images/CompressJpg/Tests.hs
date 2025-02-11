{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Images.CompressJpg.Tests
  ( tests,
  )
where

import Codec.Picture (dynamicMap, imageHeight, imageWidth)
import Codec.Picture.Jpg (decodeJpeg)
import qualified Data.ByteString as B
import Hakyll (Identifier, Item (..))
import Hakyll.Images.Internal (Image (Image, image), ImageFormat (Jpeg), loadImage)
import Hakyll.Images.CompressJpg (compressJpgCompiler)
import Hakyll.Images.Tests.Utils (testCompilerDone)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
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

compressJpg :: (Integral a) => a -> Identifier -> IO Image
compressJpg quality idt = testCompilerDone idt $ do
  Item _ compressed <- loadImage >>= compressJpgCompiler quality
  return $ compressed

-- Test that the standard Image compressed to quality 25/100 is smaller
-- than the initial image
testCompressionFromImage :: Assertion
testCompressionFromImage = do
  im <- testJpg
  let initialSize = (B.length . image) im
  finalSize <- do
    testCompilerDone "piccolo.jpg" $ do
      Item _ compressed <- loadImage >>= compressJpgCompiler (25 :: Int)
      return $ (B.length . image) compressed

  assertBool "Image was not compressed" (initialSize > finalSize)

-- Test that specifying a JPG encoding below 0 will not fail
testJpgEncodingOutOfLowerBound :: Assertion
testJpgEncodingOutOfLowerBound = do
  compressedSize <- B.length . image <$> compressJpg (-10 :: Int) "piccolo.jpg"
  expectedSize <- B.length . image <$> compressJpg (0 :: Int) "piccolo.jpg"

  assertBool "Out-of-bounds JpgQuality was not handled properly" (expectedSize == compressedSize)

-- Test that specifying a JPG encoding above 100 will fail
testJpgEncodingOutOfUpperBound :: Assertion
testJpgEncodingOutOfUpperBound = do
  compressedSize <- B.length . image <$> compressJpg (150 :: Int) "piccolo.jpg"
  expectedSize <- B.length . image <$> compressJpg (100 :: Int) "piccolo.jpg"

  assertBool "Out-of-bounds JpgQuality was not handled properly" (expectedSize == compressedSize)

-- Test that issue #11 is fixed
testJpgInadvertentRotation :: TestTree
testJpgInadvertentRotation = testCase "Compressing a JPEG does not rotate it (issue #11)" $ do
  im <- B.readFile "tests/data/issue11.jpeg"

  let initial = either error id $ decodeJpeg im
  compressed <- either error id <$> (compressJpg (50 :: Int) "issue11.jpeg" >>= pure . decodeJpeg . image)

  assertEqual mempty (dynamicMap imageWidth initial) (dynamicMap imageWidth compressed)
  assertEqual mempty (dynamicMap imageHeight initial) (dynamicMap imageHeight compressed)

tests :: TestTree
tests =
  testGroup "Hakyll.Images.CompressJpg.Tests" $
    [testJpgInadvertentRotation]
      <> fromAssertions
        "compressJpg"
        [ testCompressionFromImage,
          testJpgEncodingOutOfLowerBound,
          testJpgEncodingOutOfUpperBound
        ]
