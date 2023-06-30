--------------------------------------------------------------------------------
module Hakyll.Images.CompressJpg.Tests
  ( tests,
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import Codec.Picture (dynamicMap, imageWidth, imageHeight, imageHeight)
import Codec.Picture.Jpg (decodeJpeg)
import qualified Data.ByteString as B
import Hakyll.Images.Common ( Image(Image, image), ImageFormat(Jpeg) )
import Hakyll.Images.CompressJpg ( compressJpg )
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

-- Test that the standard Image compressed to quality 25/100 is smaller
-- than the initial image
testCompressionFromImage :: Assertion
testCompressionFromImage = do
  im <- testJpg
  let initialSize = (B.length . image) im
      finalSize = (B.length . image . compressJpg (25::Int)) im

  assertBool "Image was not compressed" (initialSize > finalSize)

-- Test that specifying a JPG encoding below 0 will not fail
testJpgEncodingOutOfLowerBound :: Assertion
testJpgEncodingOutOfLowerBound = do
  im <- testJpg
  
  let compressedSize = (B.length . image . compressJpg (-10 :: Int)) im
      expectedSize = (B.length . image . compressJpg (0 :: Int)) im

  assertBool "Out-of-bounds JpgQuality was not handled properly" (expectedSize == compressedSize)

-- Test that specifying a JPG encoding above 100 will fail
testJpgEncodingOutOfUpperBound :: Assertion
testJpgEncodingOutOfUpperBound = do
  im <- testJpg
  
  let compressedSize = (B.length . image . compressJpg (150 :: Int)) im
      expectedSize = (B.length . image . compressJpg (100 :: Int)) im

  assertBool "Out-of-bounds JpgQuality was not handled properly" (expectedSize == compressedSize)

-- Test that issue #11 is fixed
testJpgInadvertentRotation :: TestTree
testJpgInadvertentRotation = testCase "Compressing a JPEG does not rotate it (issue #11)" $ do
  im <- Image Jpeg <$> B.readFile "tests/data/issue11.jpeg"

  let initial    = either error id $ decodeJpeg (image im)
      compressed = either error id $ decodeJpeg $ image $ compressJpg (50::Int) im

  assertEqual mempty (dynamicMap imageWidth initial) (dynamicMap imageWidth compressed)
  assertEqual mempty (dynamicMap imageHeight initial) (dynamicMap imageHeight compressed)

--------------------------------------------------------------------------------
tests :: TestTree
tests =
  testGroup "Hakyll.Images.CompressJpg.Tests" $
    [testJpgInadvertentRotation] <>
      fromAssertions
          "compressJpg"
          [ testCompressionFromImage,
            testJpgEncodingOutOfLowerBound,
            testJpgEncodingOutOfUpperBound
          ]
