{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Buck2 (
    Build,
    BuildState(..),
    build,
    mkExternalExecutor,
    WrappedKey(..),
    printBuildStats,
    evalBuild,
    -- New exports for debugging
    setVerbosity,
    getVerbosity,
    Verbosity(..),

    GToJSON(..),
    GFromJSON(..),
    ExternalExecutor(..)
) where

import Control.Monad.State
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.GADT.Show
import qualified Data.Map as Map
import Data.List (sortBy)
import Control.Monad (forM_, when)
import Data.Type.Equality
import Data.HFunctor.HTraversable
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), withObject, Value, (.:), encode, decode)
import Data.Text (Text)
import Data.Some
import Data.Aeson.Types (Parser)
import System.Process (readProcess)
import System.Exit (ExitCode(..), exitWith)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Singletons
import Unsafe.Coerce (unsafeCoerce)
import System.IO (hPutStrLn, stderr)

class GFromJSON f where
  gParseJSON :: Text -> Value -> Parser (Some f)
  gParseKnownJSON :: Sing a -> Value -> Parser (f a)

instance (GFromJSON f) => FromJSON (Some f) where
  parseJSON = withObject "Some f" $ \obj -> do
    tag   <- obj .: "tag"
    value <- obj .: "value"
    gParseJSON tag value

-- Class for GADT serialization
class GToJSON f where
  gToJSON :: f a -> Value
  gToJSONTagged :: f a -> (Text, Value)  -- Returns a tag + value

instance (GToJSON f) => ToJSON (Some f) where
  toJSON (Some x) =
    let (tag, val) = gToJSONTagged x
    in object ["tag" .= tag, "value" .= val]



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

-- | ToJSON instance for WrappedKey
instance (GToJSON (k (WrappedKey k))) => ToJSON (WrappedKey k a) where
    toJSON key = gToJSON (unwrapKey key)

instance GToJSON (k (WrappedKey k)) => GToJSON (WrappedKey k) where
    gToJSON (WrappedKey k) = gToJSON k

    gToJSONTagged (WrappedKey k) = gToJSONTagged k


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
build :: forall k r a . (GCompare (WrappedKey k), GShow (WrappedKey k), HTraversable k) =>
      -- How to compute values
      (forall z . k r z -> IO (r z))
      -> WrappedKey k a -> Build (WrappedKey k) r (r a)
build compute key = do
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
            ds' <- htraverse (build compute) (unwrapKey key)
            debug Debug "Computing result for" key
            res <- liftIO $ compute ds'
            modify $ \st' -> st' { results = DMap.insert key res (results st') }
            debug Verbose "Completed build for" key
            return res

data ExternalExecutor k r = ExternalExecutor { exe_orchestrator :: forall a . FilePath -> [String] ->  k r a -> IO (r a)
                                             , exe_executor :: (forall z . k r z -> IO (r z)) -> String -> IO () }

-- | Build a value for a key using an external executable
mkExternalExecutor :: forall k r . (GToJSON (k r), GFromJSON (k r)
                                     ,GToJSON r, GFromJSON r, GShow (k r)) =>
                    ExternalExecutor k r
mkExternalExecutor =
    let queryKey :: forall a. FilePath -> [String] -> k r a -> IO (r a)
        queryKey = \exePath exeArgs key -> do
          let keyJson = BSL8.unpack $ encode $ toJSON (Some key)

          liftIO $ do
            output <- readProcess exePath (exeArgs ++ [keyJson]) ""
            hPutStrLn stderr $ "Output: " ++ output
            case decode @(Some r) (BSL8.pack output) of
                Just (Some result) -> do
                    return (unsafeCoerce result)
                Nothing -> do
                    error "Decode failure"

        performBuild :: (forall z . k r z -> IO (r z)) -> String -> IO ()
        performBuild = \compute keyJson -> do
                case decode (BSL8.pack keyJson) of
                  Just ((Some key') :: Some (k r)) -> do
                    hPutStrLn stderr $ "Building key: " ++ gshow key'
                    result <- compute key'
                    BSL8.putStrLn $ encode $ toJSON (Some result)
                  Nothing -> do
                    putStrLn "Failed to decode key from JSON"
                    exitWith (ExitFailure 1)
  in ExternalExecutor queryKey performBuild

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