--------------------------------------------------------------------------------
module Hakyll.Images.CompressJpg.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (Assertion, assertBool, assertFailure, testCase)


--------------------------------------------------------------------------------
import           Hakyll.Images.CompressJpg
import qualified Data.ByteString.Lazy   as B

import           Control.Exception      (ErrorCall, catch, evaluate)

import           Text.Printf            (printf)


testJpg :: IO B.ByteString 
testJpg = B.readFile "tests/data/piccolo.jpg"

fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [TestTree]   -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

-- Test that the standard Image compressed to quality 25/100 is smaller
-- than the initial image
testCompressionFromImage :: Assertion
testCompressionFromImage = do
    image <- testJpg
    let initialSize = B.length image
        finalSize   = B.length $ compressJpg 25 image
    
    assertBool "Image was not compressed" (initialSize > finalSize)

-- Test that specifying a JPG encoding below 0 will fail
testJpgEncodingOutOfLowerBound :: Assertion
testJpgEncodingOutOfLowerBound = do
    image <- testJpg
    -- Catching exceptions is an idea from here:
    -- https://stackoverflow.com/questions/46330592/is-it-possible-to-assert-an-error-case-in-hunit
    -- Since compressJpg is a "pure" function, we need to evaluate it in an IO context
    -- to catch errors.
    errored <- catch (evaluate (compressJpg (-10) image) >> pure False) handler
    if errored then
        pure ()
    else 
        assertFailure "did not catch expected error"
    where
        handler :: ErrorCall -> IO Bool
        handler _ = pure True

-- Test that specifying a JPG encoding above 100 will fail
testJpgEncodingOutOfUpperBound :: Assertion
testJpgEncodingOutOfUpperBound = do
    image <- testJpg
    -- Catching exceptions is an idea from here:
    -- https://stackoverflow.com/questions/46330592/is-it-possible-to-assert-an-error-case-in-hunit
    -- Since compressJpg is a "pure" function, we need to evaluate it in an IO context
    -- to catch errors.
    errored <- catch (evaluate (compressJpg 111 image) >> pure False) handler
    if errored then
        pure ()
    else 
        assertFailure "did not catch expected error"
    where
        handler :: ErrorCall -> IO Bool
        handler _ = pure True

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.CompressJpg.Tests" $ concat
   [ fromAssertions "compressJpg" 
        [ testCompressionFromImage 
        , testJpgEncodingOutOfLowerBound
        , testJpgEncodingOutOfUpperBound
        ] 
    ]