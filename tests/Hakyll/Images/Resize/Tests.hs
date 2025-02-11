{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Images.Resize.Tests
  ( tests,
  )
where

import Codec.Picture
  ( convertRGBA8,
    decodeJpeg,
    dynamicMap,
    imageHeight,
    imageWidth,
  )
import qualified Data.ByteString as B
import Data.Ratio ((%))
import Hakyll (Item (Item))
import Hakyll.Images
import Hakyll.Images.Internal (Image (Image), ImageContent, WithMetadata (..), decodeContent)
import Hakyll.Images.Tests.Utils
import Test.HUnit.Approx (assertApproxEqual)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Text.Printf (printf)

-- Original test image "piccolo.jpg" has shape 1170 x 647px
testJpg :: IO ImageContent
testJpg = decodeContent <$> B.readFile "tests/data/piccolo.jpg"

testGif :: IO ImageContent
testGif = decodeContent <$> B.readFile "tests/data/donkey.gif"

fromAssertions ::
  -- | Name
  String ->
  -- | Cases
  [Assertion] ->
  -- | Result tests
  [TestTree]
fromAssertions name =
  zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

-- Test that the rescaled image is of the appropriate scale
testResizeJpg :: Assertion
testResizeJpg = do
  image <- testJpg
  let scaledImage = resize 48 64 image
      MkWithMetadata converted _ = fmap convertRGBA8 scaledImage
      (width, height) =
        ( imageWidth converted,
          imageHeight converted
        )
  assertEqual "Image width was not resized properly" width 48
  assertEqual "Image height was not resized properly" height 64

-- Test that the rescaled image is of the appropriate scale
testResizeGif :: Assertion
testResizeGif = do
  image <- testGif
  let scaledImage = resize 48 64 image
      MkWithMetadata converted _ = fmap convertRGBA8 scaledImage
      (width, height) =
        ( imageWidth converted,
          imageHeight converted
        )
  assertEqual "Image width was not resized properly" width 48
  assertEqual "Image height was not resized properly" height 64

-- Test that the rescaled image fits in the appropriate box
testScale :: Assertion
testScale = do
  image <- testJpg
  let scaledImage = scale 600 400 image
      MkWithMetadata converted _ = fmap convertRGBA8 scaledImage
      (width, height) =
        ( imageWidth converted,
          imageHeight converted
        )
  assertBool "Image width was not scaled properly" (width <= 600)
  assertBool "Image height was not scaled properly" (height <= 400)
  assertBool "Image overall was not scaled properly" (width == 600 || height == 400)

-- Test that the images that already fit in dimensions are not scaled
testEnsureFit :: Assertion
testEnsureFit = do
  image <- testJpg
  let (originalWidth, originalHeight) = (imageWidth . convertRGBA8 . getData $ image, imageHeight . convertRGBA8 . getData $ image)
      scaledImage = ensureFit 2000 2000 image
      MkWithMetadata converted _ = fmap convertRGBA8 scaledImage
      (width, height) =
        ( imageWidth converted,
          imageHeight converted
        )
  assertEqual "Image width was not scaled properly" width originalWidth
  assertEqual "Image height was not scaled property" height originalHeight

-- Test that the rescaled image has the same aspect ratio
testScalePreservesAspectRatio :: Assertion
testScalePreservesAspectRatio = do
  image <- testJpg

  let initialAspectRatio = imageWidth (convertRGBA8 $ getData image) % imageHeight (convertRGBA8 $ getData image)
      scaledImage = scale 600 400 image
      finalAspectRatio = imageWidth (convertRGBA8 $ getData scaledImage) % imageHeight (convertRGBA8 $ getData scaledImage)

  assertApproxEqual "Aspect ratio was not preserved" 0.02 initialAspectRatio finalAspectRatio

-- Test that issue #11 is fixed
testJpgInadvertentRotation :: TestTree
testJpgInadvertentRotation = testCase "resizing a JPEG does not rotate it (issue #11)" $ do
  Item _ (Image _ raw) <-
    testCompilerDone "issue11-2.jpg" $ loadImage

  let original = either error id $ decodeJpeg raw
      initialWidth = dynamicMap imageWidth original
      initialHeight = dynamicMap imageHeight original

  Item _ (Image _ bts) <-
    testCompilerDone "issue11-2.jpg" $ loadImage >>= ensureFitCompiler 400 400

  let resized = either error id $ decodeJpeg bts
      measuredWidth = dynamicMap imageWidth resized
      measuredHeight = dynamicMap imageHeight resized

  -- The image *appears* to be w=300, h=400,
  -- but an exif tag that rotates it means that we expect
  -- the height to be smaller than the width
  assertBool mempty (initialHeight < initialWidth)
  -- The same must hold for the resized image
  assertBool mempty (measuredHeight < measuredWidth)

tests :: TestTree
tests =
  testGroup "Hakyll.Images.Resize.Tests" $
    concat
      [ fromAssertions
          "rescale"
          [ testScale,
            testScalePreservesAspectRatio
          ],
        fromAssertions
          "ensureFit"
          [ testEnsureFit
          ],
        fromAssertions
          "resize"
          [testResizeJpg, testResizeGif],
        [testJpgInadvertentRotation]
      ]
