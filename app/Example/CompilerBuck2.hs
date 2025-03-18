{-# LANGUAGE GADTs #-}
module Example.CompilerBuck2 where

import Abstract.CompilerIO

import System.Buck2
import Abstract.Operations

-- | Define how to build each type of key
--computeValue :: Key a -> Build _ _
--computeValue (ModuleKey name) =  compileModule wrapper (ModuleKey name)
--computeValue ModuleGraphKey = discoverModuleGraph wrapper ModuleGraphKey

-- | Dispatch function to handle different key types
computeValue :: Dependencies Result a -> Key a -> IO (Result a)
computeValue deps (ModuleKey name) = ResultModule <$> compileModule deps (ModuleKey name)
computeValue deps ModuleGraphKey = ResultModuleGraph <$> discoverModuleGraph deps ModuleGraphKey

wrapper :: Operations (BuildAction Key Dependencies) Result (Build Key Dependencies Result)
wrapper = Operations {
    fetch = \ba -> build hoistDependencies computeValue ba
    , fetches = mapM (build hoistDependencies computeValue)
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
