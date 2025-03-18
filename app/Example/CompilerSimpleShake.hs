{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Example.CompilerSimpleShake where

import Abstract.Compiler
import System.SimpleShake (Build, build, printBuildStats, evalBuild)

-- | Define how to build each type of key
computeValue :: Key a -> Build Key a
computeValue (ModuleKey name) = compileModule wrapper (ModuleKey name)
computeValue ModuleGraphKey = discoverModuleGraph wrapper ModuleGraphKey

wrapper :: forall x. Key x -> Build Key x
wrapper = build computeValue

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