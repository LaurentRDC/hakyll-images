--------------------------------------------------------------------------------
module Hakyll.Images.CompressJpg.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (Assertion, assertBool, testCase)


--------------------------------------------------------------------------------
import           Hakyll.Images.CompressJpg
import qualified Data.ByteString.Lazy   as B

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

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.CompressJpg.Tests" $ concat
   [ fromAssertions "compressJpg" 
        [ testCompressionFromImage ] 
    ]