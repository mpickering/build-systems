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
    evalBuild,
    -- New exports for debugging
    setVerbosity,
    getVerbosity,
    Verbosity(..)
) where

import Control.Monad.State
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.GADT.Show
import qualified Data.Map as Map
import Data.List (sortBy)
import Control.Monad (forM_, when)

-- | Verbosity level for build tracing
data Verbosity = Silent | Normal | Verbose | Debug
    deriving (Show, Eq, Ord)

-- | The Build state
data BuildState k d r = BuildState
    { results :: DMap.DMap k r       -- Cache of computed results
    , accessCounts :: Map.Map String Int   -- Track number of times each key is accessed
    , verbosity :: Verbosity         -- Verbosity level for logging
    }

-- | The Build monad
newtype Build k d r a = Build { runBuild :: StateT (BuildState k d r) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (BuildState k d r))

-- | MonadFail instance for the Build monad
instance MonadFail (Build k d r) where
    fail msg = Build $ lift $ fail msg

-- Something which we can build.
data BuildAction k d a = BuildAction { build_key :: k a, build_deps :: d (BuildAction k d) a }

-- | Debug logging function
debug :: GShow k => Verbosity -> String -> k a -> Build k d r ()
debug minLevel msg key = do
    currentLevel <- gets verbosity
    when (currentLevel >= minLevel) $
        liftIO $ putStrLn $ "[" ++ show currentLevel ++ "] " ++ msg ++ ": " ++ gshow key

-- | Build a value for a key, using the provided function to compute it if needed
build :: (GCompare k, GShow k) =>
      -- Evaluate the dependency structure
      (forall m k1 k2 z . Monad m => (forall b . k1 b -> m (k2 b)) -> d k1 z -> m (d k2 z)) ->
      -- How to compute values
      (forall z . d r z -> k z -> IO (r z))
      -> BuildAction k d a -> Build k d r (r a)
build eval_deps compute (BuildAction key deps) = do
    debug Verbose "Starting build for" key
    st <- get
    case DMap.lookup key (results st) of
        Just res -> do
              -- Increment access counter for this key
            let keyStr = gshow key
                newCount = Map.findWithDefault 0 keyStr (accessCounts st) + 1
            modify $ \s -> s { accessCounts = Map.insert keyStr newCount (accessCounts s) }
            debug Normal "Cache hit for" key
            return res
        Nothing -> do
            debug Normal "Cache miss for" key
            debug Debug "Evaluating dependencies for" key
            ds' <- eval_deps (build eval_deps compute) deps
            debug Debug "Computing result for" key
            res <- liftIO $ compute ds' key
            modify $ \st' -> st' { results = DMap.insert key res (results st') }
            debug Verbose "Completed build for" key
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

-- | Set the verbosity level for debugging
setVerbosity :: Verbosity -> Build k d r ()
setVerbosity level = modify $ \st -> st { verbosity = level }

-- | Get the current verbosity level
getVerbosity :: Build k d r Verbosity
getVerbosity = gets verbosity

-- | Evaluate a build action with an empty initial state
evalBuild :: Build k d r a -> IO a
evalBuild buildAction =
    evalStateT (runBuild buildAction) (BuildState DMap.empty Map.empty Normal)