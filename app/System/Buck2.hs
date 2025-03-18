{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Buck2 (
    Build,
    BuildState(..),
    build,
    WrappedKey(..),
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
import Data.Type.Equality

-- | Verbosity level for build tracing
data Verbosity = Silent | Normal | Verbose | Debug
    deriving (Show, Eq, Ord)

-- | The Build state
data BuildState k r = BuildState
    { results :: DMap.DMap k r       -- Cache of computed results
    , accessCounts :: Map.Map String Int   -- Track number of times each key is accessed
    , verbosity :: Verbosity         -- Verbosity level for logging
    }

-- | The Build monad
newtype Build k r a = Build { runBuild :: StateT (BuildState k r) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (BuildState k r))

-- | MonadFail instance for the Build monad
instance MonadFail (Build k r) where
    fail msg = Build $ lift $ fail msg


-- | Debug logging function
debug :: GShow k => Verbosity -> String -> k a -> Build k' r ()
debug minLevel msg key = do
    currentLevel <- gets verbosity
    when (currentLevel >= minLevel) $
        liftIO $ putStrLn $ "[" ++ show currentLevel ++ "] " ++ msg ++ ": " ++ gshow key

-- This WrappedKey business is because, the identify of a rule depends on what the argument
-- are, and so-on recursively.

-- In build systems like Buck2, if any of the structure of the transitive dependencies
-- change then the rule is considered different.

-- So we need to wrap the key in a way that preserves the structure of the dependencies
-- so that we can compare them.

data WrappedKey k a = WrappedKey { unwrapKey :: k (WrappedKey k) a }

-- | GEq instance for WrappedKey
instance GEq (k (WrappedKey k)) => GEq (WrappedKey k) where
    geq (WrappedKey a) (WrappedKey b) = do
        Refl <- geq a b
        return Refl

-- | GCompare instance for WrappedKey
instance GCompare (k (WrappedKey k)) => GCompare (WrappedKey k) where
    gcompare (WrappedKey a) (WrappedKey b) = case gcompare a b of
        GLT -> GLT
        GEQ -> GEQ
        GGT -> GGT

-- | Eq instance for WrappedKey
instance (GCompare (k (WrappedKey k))) => Eq (WrappedKey k a) where
    -- Two wrapped keys are equal if they compare as GEQ
    -- This is a default implementation that relies on GCompare
    (==) a b = case gcompare a b of
        GEQ -> True
        _   -> False

-- | Ord instance for WrappedKey
instance (GCompare (k (WrappedKey k))) => Ord (WrappedKey k a) where
    -- Compare wrapped keys using GCompare
    compare a b = case gcompare a b of
        GLT -> LT
        GEQ -> EQ
        GGT -> GT

-- | GShow instance for WrappedKey
instance GShow (k (WrappedKey k)) => GShow (WrappedKey k) where
    gshowsPrec p (WrappedKey k) = showParen (p > 10) $
        showString "WrappedKey " . gshowsPrec 11 k


{- Note the two critical differences between a buck2/nix system and shake system

1. In a shake system, the function passed to build to compute values (ie perform compilation
   runs in the 'Build' monad.

   In a buck2/nix system, the function passed to build to compute values runs in the 'IO' monad.

   Therefore, when shake executes a compilation rule, it can therefore suspend computing the
   value and pass control back to the build system.

   In buck2/nix, once the build actions start being computed, they can't be suspended.


2. In a buck2/nix system, dependencies of rules must be specified before the rule is executed.

   In a shake system, dependencies can be discovered as a rule is being executed.

   In the model, a buck2/nix system uses a key which which can contain references to
   other keys. This fixpoint is closed by the WrappedKey function. Before a rule is
   executed, the WrappedKey is unwrapped and the dependencies are computed.

   In the buck2/nix system, giving the rules names would add a layer of indirection which
   would make the fix-point unecessary.

   In the shake model, you do not need to include the dependencies in the key, since
   whilst the build is being executed you can dynamically add need dependencies by
   `need`ing.

   In buck2/nix, dependencies for a rule are known BEFORE executation starts.Build

   In the shake model, dependencies are known AFTER execution finishes.
-}



-- | Build a value for a key, using the provided function to compute it if needed
build :: forall k r a . (GCompare (WrappedKey k), GShow (WrappedKey k)) =>
      -- Evaluate the dependency structure
      (forall m k1 k2 z . Monad m => (forall b . k1 b -> m (k2 b)) -> k k1 z -> m (k k2 z)) ->
      -- How to compute values
      (forall z . k r z -> IO (r z))
      -> WrappedKey k a -> Build (WrappedKey k) r (r a)
build eval_deps compute key = do
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
            ds' <- eval_deps (build eval_deps compute) (unwrapKey key)
            debug Debug "Computing result for" key
            res <- liftIO $ compute ds'
            modify $ \st' -> st' { results = DMap.insert key res (results st') }
            debug Verbose "Completed build for" key
            return res

-- | Print build statistics
printBuildStats :: Build k r ()
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
setVerbosity :: Verbosity -> Build k r ()
setVerbosity level = modify $ \st -> st { verbosity = level }

-- | Get the current verbosity level
getVerbosity :: Build k r Verbosity
getVerbosity = gets verbosity

-- | Evaluate a build action with an empty initial state
evalBuild :: Build k r a -> IO a
evalBuild buildAction =
    evalStateT (runBuild buildAction) (BuildState DMap.empty Map.empty Normal)