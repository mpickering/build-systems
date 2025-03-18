{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- An example of a build system like shake, with named rules.
module Example.SimpleShake where

import Abstract.Simple
import System.Shake (Build, rule, build, printBuildStats, evalBuild)
import Data.GADT.Compare
import Data.Type.Equality

data RuleMatch o where
    MatchModule :: RuleMatch FilePath
    MatchModuleGraph :: RuleMatch [Module]

instance GEq RuleMatch where
    geq MatchModule MatchModule = Just Refl
    geq MatchModuleGraph MatchModuleGraph = Just Refl
    geq _ _ = Nothing

instance GCompare RuleMatch where
    gcompare MatchModule MatchModule = GEQ
    gcompare MatchModuleGraph MatchModuleGraph = GEQ
    gcompare MatchModule MatchModuleGraph = GLT
    gcompare MatchModuleGraph MatchModule = GGT

-- | Map a Key to its corresponding RuleMatch
keyToMatch :: Key a -> RuleMatch a
keyToMatch (ModuleKey _) = MatchModule
keyToMatch ModuleGraphKey = MatchModuleGraph

-- | Run the SimpleBuildExample with the Build monad
runExampleBuild :: IO ()
runExampleBuild = evalBuild $ do
    -- Register rules
    rule MatchModuleGraph (discoverModuleGraph buildWrapper)
    rule MatchModule (compileModule buildWrapper)

    -- Run the build process
    exampleBuild buildWrapper

    -- Print build statistics
    printBuildStats
  where
    buildWrapper :: forall a. Key a -> Build RuleMatch Key a
    buildWrapper k = do
      result <- build keyToMatch k
      case result of
        Just a -> return a
        Nothing -> error $ "Failed to build key: " ++ keyToString k

-- Optionally add a main function for testing
main :: IO ()
main = runExampleBuild
