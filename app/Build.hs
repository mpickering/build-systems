{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Build (
    Build(..),
    BuildState(..),
    RuleResult(..),
    rule,
    findRule,
    build,
    printBuildStats,
    evalBuild
) where

import Control.Monad.State
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Control.Monad (forM_)
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Functor.Classes
import Data.GADT.Show

-- | Abstracting over the rule match type
data RuleResult r k o = RuleResult { getRuleResult :: k o -> Build r k o }

-- | The Build monad, now abstract over the key type k
data BuildState r k = BuildState
    { rules :: DMap.DMap r (RuleResult r k) -- Dependent map for rules
    , results :: DMap.DMap k Identity
    , accessCounts :: Map.Map String Int      -- Track number of times each key is accessed
    }

newtype Build r k a = Build { runBuild :: StateT (BuildState r k) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (BuildState r k))

-- | MonadFail instance for the Build monad
instance MonadFail (Build r k) where
    fail msg = Build $ lift $ fail msg

-- | Define a rule with a matching pattern
rule :: GCompare r => r o -> (k o -> Build r k o) -> Build r k ()
rule match action = modify $ \st ->
    st { rules = DMap.insert match (RuleResult action) (rules st) }

-- | Find a rule matching a given key
findRule :: GCompare r => (k o -> r o) -> k o -> Build r k (Maybe (k o -> Build r k o))
findRule keyToMatch key = do
    st <- get
    return $ fmap getRuleResult (DMap.lookup (keyToMatch key) (rules st))

-- | Check if a result exists and execute the rule if needed
build :: (GCompare k, GCompare r, GShow k) => (forall o. k o -> r o) -> k a -> Build r k (Maybe a)
build keyToMatch key = do
    st <- get
    case DMap.lookup key (results st) of
        Just (Identity res) -> do
            -- Increment access counter for this key
            let keyStr = gshow key
                newCount = Map.findWithDefault 0 keyStr (accessCounts st) + 1
            modify $ \s -> s { accessCounts = Map.insert keyStr newCount (accessCounts s) }
            return (Just res)
        Nothing -> do
            ruleOpt <- findRule keyToMatch key
            case ruleOpt of
                Just action -> do
                    res <- action key
                    modify $ \st' -> st' { results = DMap.insert key (Identity res) (results st') }
                    return (Just res)
                Nothing -> return Nothing

-- | Print build statistics
printBuildStats :: Build r k ()
printBuildStats = do
    st <- get
    liftIO $ putStrLn "\nBuild Cache Statistics:"
    liftIO $ putStrLn "========================"
    let stats = Map.toList (accessCounts st)
    if null stats
        then liftIO $ putStrLn "No cached results were accessed."
        else forM_ (sortByCount stats) $ \(key, count) ->
            liftIO $ putStrLn $ key ++ ": accessed " ++ show count ++ " time(s)"
  where
    sortByCount = sortBy (\(_, c1) (_, c2) -> compare c2 c1)

-- | Evaluate a build action with an empty initial state
evalBuild :: Build r k a -> IO a
evalBuild buildAction =
    evalStateT (runBuild buildAction) (BuildState DMap.empty DMap.empty Map.empty)