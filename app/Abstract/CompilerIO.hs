{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
module Abstract.CompilerIO where

import Control.Monad.State
import Data.GADT.Compare
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Type.Equality
import Data.GADT.Show
import Control.Concurrent (threadDelay)
import Abstract.Operations
import Control.Monad.Identity
import Data.Functor.Compose
import System.Buck2

type ModuleName = String

type ModuleInterface = String

type data Rules = ModuleGraphRule | ModuleRule

-- NB: In this example, all dependencies are in the key.
-- Similar to the content-addressed build system.
data Key a where
    ModuleKey :: ModuleName -> Key ModuleRule
    ModuleGraphKey :: Key ModuleGraphRule -- Rule type for computing the module graph

data Dependencies k a where
    DependenciesModule :: [k ModuleRule] -> Dependencies k ModuleRule
    DependenciesModuleGraph :: Dependencies k ModuleGraphRule

-- | Lift a transformation of keys to a transformation of dependencies
hoistDependencies :: Monad m => (forall b. k b -> m (k' b)) -> Dependencies k a -> m (Dependencies k' a)
hoistDependencies f (DependenciesModule ks) = do
  ks' <- traverse f ks
  return (DependenciesModule ks')
hoistDependencies _ DependenciesModuleGraph =
  return DependenciesModuleGraph


data Result a where
    ResultModule :: ModuleInterface -> Result ModuleRule
    ResultModuleGraph :: [Module] -> Result ModuleGraphRule

-- | Show instance for Key
instance Show (Key a) where
    show (ModuleKey name) = "ModuleKey " ++ show name
    show ModuleGraphKey = "ModuleGraphKey"

-- | Show instance for Dependencies
instance Show (Dependencies k a) where
    show (DependenciesModule _) = "DependenciesModule [...]"
    show DependenciesModuleGraph = "DependenciesModuleGraph"

-- | Show instance for Result
instance Show (Result a) where
    show (ResultModule interface) = "ResultModule " ++ show interface
    show (ResultModuleGraph modules) = "ResultModuleGraph " ++ show (length modules) ++ " modules"






-- | GShow instance for Key to enable pretty printing
instance GShow Key where
    gshowsPrec p (ModuleKey name) = showParen (p > 10) $
        showString "ModuleKey " . showsPrec 11 name
    gshowsPrec _ ModuleGraphKey = showString "ModuleGraphKey"


instance GEq Key where
    geq (ModuleKey a) (ModuleKey b)
        | a == b = Just Refl
        | otherwise = Nothing
    geq ModuleGraphKey ModuleGraphKey = Just Refl
    geq _ _ = Nothing

instance GCompare Key where
    gcompare (ModuleKey a) (ModuleKey b) = case compare a b of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
    gcompare ModuleGraphKey ModuleGraphKey = GEQ
    gcompare ModuleGraphKey _ = GLT
    gcompare _ ModuleGraphKey = GGT

-- | Rule matching criteria

-- | Convert a key to its string representation for stats tracking
keyToString :: Key a -> String
keyToString (ModuleKey  name) = "Module:" ++ name
keyToString ModuleGraphKey = "ModuleGraph"

-- | A simple representation of a programming language module
data Module = Module
    { moduleName :: String
    , dependencies :: [String]
    , sourceCode :: String
    } deriving (Show, Eq)

-- | Simulating a module compilation process
compileModule :: (MonadIO m, Monad m) =>
  Dependencies Result ModuleRule
  -> Key ModuleRule -> m ModuleInterface
compileModule modules (ModuleKey name) = do
        liftIO $ putStrLn ("Provisioning: " ++ show modules)
        liftIO $ threadDelay 100_000
        return $ "Compiled: " ++ name

-- | Rule for computing the module graph
discoverModuleGraph :: (Monad m) => Dependencies Result ModuleGraphRule -> Key ModuleGraphRule -> m [Module]
discoverModuleGraph DependenciesModuleGraph ModuleGraphKey = do
    let modules = [ Module "A" [] "print(\"Hello from A\")"
                  , Module "B" ["A"] "print(\"Hello from B\")"
                  , Module "C" ["A", "B"] "print(\"Hello from C\")"
                  , Module "D" ["B"] "print(\"Hello from D\")"
                  , Module "E" ["B", "C"] "print(\"Hello from E\")"
                  , Module "F" ["C"] "print(\"Hello from F\")"
                  , Module "G" ["D", "F"] "print(\"Hello from G\")"
                  , Module "H" ["E"] "print(\"Hello from H\")"
                  , Module "I" ["G", "H"] "print(\"Hello from I\")"
                  , Module "Root" ["A", "B", "C", "D", "E", "F", "G", "H", "I"] "print(\"Hello from Root\")"
                  ]
    return modules

-- | Example: Compiling a small program with dynamic dependencies
exampleBuild :: (Monad m, MonadIO m, MonadFail m)
             => Operations (BuildAction Key Dependencies) Result m
             -> m ()
exampleBuild Operations{..} = do
    (ResultModuleGraph modules) <- fetch (BuildAction ModuleGraphKey DependenciesModuleGraph)
    -- Print the module dependency tree
    -- Compile all modules
    _ <- fetches (mkBuildActions modules)

    return ()

mkBuildActions :: [Module] -> [BuildAction Key Dependencies ModuleRule]
mkBuildActions ms = Map.elems res
  where
    res = Map.fromList [(moduleName m, BuildAction (ModuleKey (moduleName m))
                        (DependenciesModule (map ((Map.!) res) (dependencies m))))
                       | m <- ms]
