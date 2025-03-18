{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.SimpleShake (
    Build,
    BuildState(..),
    build,
    printBuildStats,
    evalBuild
) where

import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.GADT.Show
import qualified Data.Map as Map
import Data.List (sortBy)
import Control.Monad (forM_)

-- | The Build state
data BuildState k = BuildState
    { results :: DMap.DMap k Identity      -- Cache of computed results
    , accessCounts :: Map.Map String Int   -- Track number of times each key is accessed
    }

-- | The Build monad
newtype Build k a = Build { runBuild :: StateT (BuildState k) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (BuildState k))

-- | MonadFail instance for the Build monad
instance MonadFail (Build k) where
    fail msg = Build $ lift $ fail msg

-- | Build a value for a key, using the provided function to compute it if needed
build :: (GCompare k, GShow k) => (k a -> Build k a) -> k a -> Build k a
build compute key = do
    st <- get
    case DMap.lookup key (results st) of
        Just (Identity res) -> do
            -- Increment access counter for this key
            let keyStr = gshow key
                newCount = Map.findWithDefault 0 keyStr (accessCounts st) + 1
            modify $ \s -> s { accessCounts = Map.insert keyStr newCount (accessCounts s) }
            return res
        Nothing -> do
            res <- compute key
            modify $ \st' -> st' { results = DMap.insert key (Identity res) (results st') }
            return res

-- | Print build statistics
printBuildStats :: Build k ()
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
evalBuild :: Build k a -> IO a
evalBuild buildAction =
    evalStateT (runBuild buildAction) (BuildState DMap.empty Map.empty)