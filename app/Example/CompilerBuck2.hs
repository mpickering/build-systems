{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Example.CompilerBuck2 (runExampleBuild, main, computeValue) where

import Abstract.CompilerIO
import System.Buck2
import Abstract.Operations
import System.Exit (exitFailure)

-- | Dispatch function to handle different key types
computeValue :: Key Result a -> IO (Result a)
computeValue (ModuleKey name deps) = ResultModule <$> compileModule (ModuleKey name deps)
computeValue ModuleGraphKey = ResultModuleGraph <$> discoverModuleGraph ModuleGraphKey

simpleWrapper :: (forall a . WrappedKey Key a -> Build (WrappedKey Key) Result (Result a)) -> Operations (WrappedKey Key) Result (Build (WrappedKey Key) Result)
simpleWrapper f = Operations {
    fetch = \ba -> f ba
    , fetches = mapM f
    }

-- | Run the SimpleBuildExample with the SimpleShake Build monad
runExampleBuild :: IO ()
runExampleBuild = evalBuild $ do
    -- No need to register rules - just build directly
    exampleBuild (simpleWrapper (build computeValue))

    -- Print build statistics
    printBuildStats

-- | Run the SimpleBuildExample with the SimpleShake Build monad, using an external executable
runExampleBuildWithExe :: (forall a . Key Result a -> IO (Result a)) -> IO ()
runExampleBuildWithExe k = do
    evalBuild $ do
      -- Use wrapperWithExe to run the build with the external executable
      exampleBuild (simpleWrapper (build k))
      -- Print build statistics
      printBuildStats

-- Command line interface for the compiler
main :: [String] -> IO ()
main args= do
    let ExternalExecutor orchestrator executor = mkExternalExecutor @Key @Result
    case args of
        ["--compute-value", keyJSON] -> executor computeValue keyJSON

        ["--run-example-with-exe", exePath] -> runExampleBuildWithExe (orchestrator exePath ["compiler-buck2", "--compute-value"])
        ["--run-example"] -> runExampleBuild

        _ -> do
            putStrLn "Usage:"
            putStrLn "  build-system --compute-value <serialized-key>"
            putStrLn "  build-system --run-example"
            exitFailure
