{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Example.CompilerSimpleShakePar (main) where

import Abstract.Compiler
import System.SimpleShakePar
import Abstract.Operations

-- | Define how to build each type of key
computeValue :: Operations Key (Build Key) -> Key a -> Build Key a
computeValue compute (ModuleKey name) = compileModule compute (ModuleKey name)
computeValue compute ModuleGraphKey = discoverModuleGraph compute ModuleGraphKey

-- | Run the SimpleBuildExample with the SimpleShake Build monad
runExampleBuild :: Int -> IO ()
runExampleBuild threads = evalBuildWithThreads threads computeValue $ \ops -> do
    -- No need to register rules - just build directly
    exampleBuild ops

    -- Print build statistics
    printBuildStats

-- Optionally add a main function for testing
main :: IO ()
main = runExampleBuild 4