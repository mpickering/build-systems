{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module System.Buck2 (
    Build,
    BuildState(..),
    BuildAction(..),
    build,
    printBuildStats,
    evalBuild
) where

import Control.Monad.State
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.GADT.Show
import qualified Data.Map as Map
import Data.List (sortBy)
import Control.Monad (forM_)

-- | The Build state
data BuildState k d r = BuildState
    { results :: DMap.DMap k r       -- Cache of computed results
    , accessCounts :: Map.Map String Int   -- Track number of times each key is accessed
    }

-- | The Build monad
newtype Build k d r a = Build { runBuild :: StateT (BuildState k d r) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (BuildState k d r))

-- | MonadFail instance for the Build monad
instance MonadFail (Build k d r) where
    fail msg = Build $ lift $ fail msg

-- Something which we can build.
data BuildAction k d a = BuildAction { build_key :: k a, build_deps :: d (BuildAction k d) a }

-- | Build a value for a key, using the provided function to compute it if needed
build :: (GCompare k, GShow k) =>
      -- Evaluate the dependency structure
      (forall m k k' a . Monad m => (forall b . k b -> m (k' b)) -> d k a -> m (d k' a)) ->
      -- How to compute values
      (forall a . d r a -> k a -> IO (r a))
      -> BuildAction k d a -> Build k d r (r a)
build eval_deps compute (BuildAction key deps) = do
    st <- get
    case DMap.lookup key (results st) of
        Just res -> do
              -- Increment access counter for this key
            let keyStr = gshow key
                newCount = Map.findWithDefault 0 keyStr (accessCounts st) + 1
            modify $ \s -> s { accessCounts = Map.insert keyStr newCount (accessCounts s) }
            return res
        Nothing -> do
            ds' <- eval_deps (build eval_deps compute) deps
            res <- liftIO $ compute ds' key
            modify $ \st' -> st' { results = DMap.insert key res (results st') }
            return res

-- | Print build statistics
printBuildStats :: Build k d r ()
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
evalBuild :: Build k d r a -> IO a
evalBuild buildAction =
    evalStateT (runBuild buildAction) (BuildState DMap.empty Map.empty)