{-# LANGUAGE GADTs #-}
module Example.CompilerBuck2 where

import Abstract.CompilerIO

import System.Buck2
import Abstract.Operations

-- | Dispatch function to handle different key types
computeValue :: Key Result a -> IO (Result a)
computeValue (ModuleKey name deps) = ResultModule <$> compileModule (ModuleKey name deps)
computeValue ModuleGraphKey = ResultModuleGraph <$> discoverModuleGraph ModuleGraphKey

wrapper :: Operations (WrappedKey Key) Result (Build (WrappedKey Key) Result)
wrapper = Operations {
    fetch = \ba -> build hoistKey computeValue ba
    , fetches = mapM (build hoistKey computeValue)
    }

-- | Run the SimpleBuildExample with the SimpleShake Build monad
runExampleBuild :: IO ()
runExampleBuild = evalBuild $ do
    -- No need to register rules - just build directly
    exampleBuild wrapper
    -- Print build statistics
    printBuildStats

-- Optionally add a main function for testing
main :: IO ()
main = runExampleBuild
