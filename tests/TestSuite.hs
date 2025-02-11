--------------------------------------------------------------------------------
module Main
  ( main,
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import qualified Hakyll.Images.Common.Tests
import qualified Hakyll.Images.CompressJpg.Tests
import qualified Hakyll.Images.Metadata.Tests
import qualified Hakyll.Images.Resize.Tests
import qualified Hakyll.Images.Tests.Utils
import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  defaultMain $
    testGroup
      "Hakyll"
      [ Hakyll.Images.Common.Tests.tests,
        Hakyll.Images.CompressJpg.Tests.tests,
        Hakyll.Images.Resize.Tests.tests,
        Hakyll.Images.Metadata.Tests.tests
      ]

  Hakyll.Images.Tests.Utils.cleanTestEnv
