--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------

import qualified Hakyll.Images.Common.Tests
import qualified Hakyll.Images.CompressJpg.Tests
import qualified Hakyll.Images.Resize.Tests
import qualified Hakyll.Images.Metadata.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hakyll"
    [ Hakyll.Images.Common.Tests.tests
    , Hakyll.Images.CompressJpg.Tests.tests
    , Hakyll.Images.Resize.Tests.tests 
    , Hakyll.Images.Metadata.Tests.tests
    ]