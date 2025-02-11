-- | Test utilities
module Hakyll.Images.Tests.Utils
  ( testCompiler,
    testCompilerDone,
    testConfiguration,
    cleanTestEnv,
  )
where

import Data.List (intercalate)
import qualified Data.Set as S
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Configuration
import Hakyll.Core.Identifier
import qualified Hakyll.Core.Logger as Logger
import Hakyll.Core.Provider
import qualified Hakyll.Core.Store as Store
import Hakyll.Core.Util.File

testCompiler ::
  Identifier ->
  Compiler a ->
  IO (CompilerResult a)
testCompiler underlying compiler = do
  store <- Store.new True $ storeDirectory testConfiguration
  provider <-
    newProvider store (const $ return False) $
      providerDirectory testConfiguration
  logger <- Logger.new Logger.Error
  let read' =
        CompilerRead
          { compilerConfig = testConfiguration,
            compilerUnderlying = underlying,
            compilerProvider = provider,
            compilerUniverse = S.empty,
            compilerRoutes = mempty,
            compilerStore = store,
            compilerLogger = logger
          }

  result <- runCompiler compiler read'
  Logger.flush logger
  return result

testCompilerDone :: Identifier -> Compiler a -> IO a
testCompilerDone underlying compiler = do
  result <- testCompiler underlying compiler
  case result of
    CompilerDone x _ -> return x
    CompilerError e ->
      fail $
        "Hakyll.Images.Tests.Utils.testCompilerDone: compiler "
          ++ show underlying
          ++ " threw: "
          ++ intercalate "; " (compilerErrorMessages e)
    CompilerRequire i _ ->
      fail $
        "Hakyll.Images.Tests.Utils.testCompilerDone: compiler "
          ++ show underlying
          ++ " requires: "
          ++ show i
    CompilerSnapshot _ _ ->
      fail
        "Hakyll.Images.Tests.Utils.testCompilerDone: unexpected CompilerSnapshot"

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
