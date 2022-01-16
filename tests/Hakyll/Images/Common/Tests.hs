{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Hakyll.Images.Common.Tests
  ( tests,
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import Hakyll
import qualified Hakyll.Core.Logger as L
import Hakyll.Core.Rules.Internal (RuleSet)
import Hakyll.Core.Runtime
import Hakyll.Images
import System.Directory (doesFileExist)
import System.Exit      (ExitCode)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Text.Printf (printf)

-- RunMode was introduced in Hakyll 4.15
runCompat :: Configuration -> L.Logger -> Rules a -> IO (ExitCode, RuleSet)
runCompat = 
#if MIN_VERSION_hakyll(4,15,0)
  run RunModeNormal 
#else
  run
#endif

fromAssertions ::
  -- | Name
  String ->
  -- | Cases
  [Assertion] ->
  -- | Result tests
  [TestTree]
fromAssertions name =
  zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

testConfiguration :: Configuration
testConfiguration =
  defaultConfiguration
    { destinationDirectory = "_testsite",
      storeDirectory = "_teststore",
      tmpDirectory = "_testtmp",
      providerDirectory = "tests/data"
    }

cleanTestEnv :: IO ()
cleanTestEnv = do
  removeDirectory $ destinationDirectory testConfiguration
  removeDirectory $ storeDirectory testConfiguration
  removeDirectory $ tmpDirectory testConfiguration

case1 :: Assertion
case1 = do
  logger <- L.new L.Error
  _ <- runCompat testConfiguration logger $ do
    match "*.jpg" $ do
      route idRoute
      compile $
        loadImage
          >>= compressJpgCompiler (50::Int)

  _ <-
    assertBool "Image was not written"
      <$> doesFileExist ( destinationDirectory testConfiguration </> "piccolo.jpg")

  cleanTestEnv

--------------------------------------------------------------------------------
tests :: TestTree
tests =
  testGroup "Hakyll.Images.Common.Tests" $ fromAssertions "run" [case1]
