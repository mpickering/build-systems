{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module RunBuildExample where

import SimpleBuildExample
import Build (Build, rule, build, printBuildStats, evalBuild)
import Control.Monad.IO.Class (MonadIO)

-- | Run the SimpleBuildExample with the Build monad
runExampleBuild :: IO ()
runExampleBuild = evalBuild $ exampleBuild ruleWrapper buildWrapper printBuildStats
  where
    ruleWrapper :: RuleMatch a -> (Key a -> Build RuleMatch Key a) -> Build RuleMatch Key ()
    ruleWrapper = rule

    buildWrapper :: forall a. Key a -> Build RuleMatch Key a
    buildWrapper k = do
      result <- build keyToMatch k
      case result of
        Just a -> return a
        Nothing -> error $ "Failed to build key: " ++ keyToString k

-- Optionally add a main function for testing
main :: IO ()
main = runExampleBuild