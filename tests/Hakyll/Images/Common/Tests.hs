{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Hakyll.Images.Common.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (Assertion, testCase, assertBool)


--------------------------------------------------------------------------------
import           Hakyll
import qualified Hakyll.Core.Logger as L
import           Hakyll.Core.Runtime
import           Hakyll.Images

import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>))
import           Text.Printf            (printf)

fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [TestTree]   -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

testConfiguration :: Configuration
testConfiguration = defaultConfiguration
    { destinationDirectory = "_testsite"
    , storeDirectory       = "_teststore"
    , tmpDirectory         = "_testtmp"
    , providerDirectory    = "tests/data"
    }

cleanTestEnv :: IO ()
cleanTestEnv = do
    removeDirectory $ destinationDirectory testConfiguration
    removeDirectory $ storeDirectory testConfiguration
    removeDirectory $ tmpDirectory testConfiguration

case1 :: Assertion
case1 = do
    logger <- L.new L.Error
    _ <- run testConfiguration logger $ do
        match "*.jpg" $ do
            route idRoute
            compile $ loadImage
                >>= compressJpgCompiler 50
    
    _ <- assertBool "Image was not written" <$> 
        (doesFileExist $ destinationDirectory testConfiguration </> "piccolo.jpg")

    cleanTestEnv


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Images.Common.Tests" $ concat
   [ fromAssertions "run" [ case1 ]
   ]