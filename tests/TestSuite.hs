--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hakyll.Images.CompressJpg.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hakyll"
    [ Hakyll.Images.CompressJpg.Tests.tests ]