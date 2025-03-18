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
import qualified Data.Map as Map
import Data.Type.Equality
import Data.GADT.Show
import Control.Concurrent (threadDelay)
import Abstract.Operations
import System.Buck2

type ModuleName = String

type ModuleInterface = String

type data Rules = ModuleGraphRule | ModuleRule

-- NB: In this example, all dependencies are in the key.
-- Similar to the content-addressed build system.
data Key k a where
    ModuleKey :: ModuleName -> [k ModuleRule] -> Key k ModuleRule
    ModuleGraphKey :: Key k ModuleGraphRule -- Rule type for computing the module graph



-- | Lift a transformation of keys to a transformation of dependencies
hoistKey :: Monad m => (forall b. k b -> m (k' b)) -> Key k a -> m (Key k' a)
hoistKey f (ModuleKey name ks) = do
  ks' <- traverse f ks
  return (ModuleKey name ks')
hoistKey _ ModuleGraphKey = return ModuleGraphKey


data Result a where
    ResultModule :: ModuleInterface -> Result ModuleRule
    ResultModuleGraph :: [Module] -> Result ModuleGraphRule

-- | Show instance for Key
instance Show (Key k a) where
    show (ModuleKey name _) = "ModuleKey " ++ show name
    show ModuleGraphKey = "ModuleGraphKey"

-- | Show instance for Result
instance Show (Result a) where
    show (ResultModule interface) = "ResultModule " ++ show interface
    show (ResultModuleGraph modules) = "ResultModuleGraph " ++ show (length modules) ++ " modules"






-- | GShow instance for Key to enable pretty printing
instance GShow (Key k) where
    gshowsPrec p (ModuleKey name _) = showParen (p > 10) $
        showString "ModuleKey " . showsPrec 11 name
    gshowsPrec _ ModuleGraphKey = showString "ModuleGraphKey"


instance Eq (k ModuleRule) => GEq (Key k) where
    geq (ModuleKey a x) (ModuleKey b y)
        | x == y && a == b = Just Refl
        | otherwise = Nothing
    geq ModuleGraphKey ModuleGraphKey = Just Refl
    geq _ _ = Nothing

instance Ord (k ModuleRule) => GCompare (Key k) where
    gcompare (ModuleKey a x) (ModuleKey b y) = case compare a b <> compare x y of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
    gcompare ModuleGraphKey ModuleGraphKey = GEQ
    gcompare ModuleGraphKey _ = GLT
    gcompare _ ModuleGraphKey = GGT

-- | Rule matching criteria

-- | Convert a key to its string representation for stats tracking
keyToString :: Key k a -> String
keyToString (ModuleKey  name _) = "Module:" ++ name
keyToString ModuleGraphKey = "ModuleGraph"

-- | A simple representation of a programming language module
data Module = Module
    { moduleName :: String
    , dependencies :: [String]
    , sourceCode :: String
    } deriving (Show, Eq)

-- | Simulating a module compilation process
compileModule :: (MonadIO m, Monad m) =>
  Key Result ModuleRule -> m ModuleInterface
compileModule (ModuleKey name modules) = do
        liftIO $ putStrLn ("Provisioning: " ++ show modules)
        liftIO $ threadDelay 100_000
        return $ "Compiled: " ++ name

-- | Rule for computing the module graph
discoverModuleGraph :: (Monad m) => Key Result ModuleGraphRule -> m [Module]
discoverModuleGraph ModuleGraphKey = do
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
             => Operations (WrappedKey Key) Result m
             -> m ()
exampleBuild Operations{..} = do
    (ResultModuleGraph modules) <- fetch (WrappedKey ModuleGraphKey)
    -- Print the module dependency tree
    -- Compile all modules
    _ <- fetches (mkBuildActions modules)

    return ()

mkBuildActions :: [Module] -> [WrappedKey Key ModuleRule]
mkBuildActions ms = Map.elems res
  where
    res = Map.fromList [(moduleName m, WrappedKey (ModuleKey (moduleName m) (map ((Map.!) res) (dependencies m))))
                       | m <- ms]
