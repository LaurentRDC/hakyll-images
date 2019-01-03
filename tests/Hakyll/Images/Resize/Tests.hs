--------------------------------------------------------------------------------
module Hakyll.Images.Resize.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (Assertion, assertBool, assertEqual, testCase)
import           Test.HUnit.Approx      (assertApproxEqual)


--------------------------------------------------------------------------------
import           Hakyll.Images.Resize
import qualified Data.ByteString        as B
import           Data.Ratio             ((%))

import           Codec.Picture

import           Text.Printf            (printf)

-- Original test image "piccolo.jpg" has shape 1170 x 647px
testJpg :: IO DynamicImage
testJpg = do
    img <- decodeJpeg <$> B.readFile "tests/data/piccolo.jpg"
    case img of
        Left _ -> error "Could not decode test picture piccolo.jpg"
        Right im -> return im

fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [TestTree]   -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

-- Test that the rescaled image is of the appropriate scale
testResize :: Assertion
testResize = do
    image <- testJpg
    let scaledImage = resize 48 64 image
        converted = convertRGBA8 scaledImage
        (width, height) = ( imageWidth converted
                          , imageHeight converted
                          )
    assertEqual "Image width was not resized properly" width 48 
    assertEqual "Image height was not resized properly" height 64

-- Test that the rescaled image fits in the appropriate box
testScale :: Assertion
testScale = do
    image <- testJpg
    let scaledImage = scale 600 400 image
        converted = convertRGBA8 scaledImage
        (width, height) = ( imageWidth converted
                          , imageHeight converted
                          )
    assertEqual "Image width was not scaled properly" width 600
    assertBool "Image height was not scaled property" (height<= 400)

-- Test that the rescaled image has the same aspect ratio
testScalePreservesAspectRatio :: Assertion
testScalePreservesAspectRatio = do
    image <- testJpg

    let initialAspectRatio = (imageWidth $ convertRGBA8 image) % (imageHeight $ convertRGBA8 image)
        scaledImage = scale 600 400 image
        finalAspectRatio = (imageWidth $ convertRGBA8 scaledImage) % (imageHeight $ convertRGBA8 scaledImage)
    
    assertApproxEqual "Aspect ratio was not preserved" 0.02 initialAspectRatio finalAspectRatio
        

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Images.Resize.Tests" $ concat
   [ fromAssertions "rescale" 
        [ testResize 
        , testScale
        , testScalePreservesAspectRatio
        ] 
    ]