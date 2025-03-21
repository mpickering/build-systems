{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Abstract.Compiler where

import Control.Monad.State
import Data.GADT.Compare
import Control.Monad (forM_)
import Data.Type.Equality
import Data.GADT.Show
import Control.Concurrent (threadDelay)
import Abstract.Operations
import Control.Monad.Identity

type ModuleName = String

-- | A key type for dependent map
data Key a where
    ModuleKey :: ModuleName -> Key FilePath
    ModuleGraphKey :: Key [Module] -- Rule type for computing the module graph

-- | GShow instance for Key to enable pretty printing
instance GShow Key where
    gshowsPrec p (ModuleKey name) = showParen (p > 10) $
        showString "ModuleKey " . showsPrec 11 name
    gshowsPrec _ ModuleGraphKey = showString "ModuleGraphKey"


instance GEq Key where
    geq (ModuleKey a) (ModuleKey b)
        | a == b    = Just Refl
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
keyToString (ModuleKey name) = "Module:" ++ name
keyToString ModuleGraphKey = "ModuleGraph"

-- | A simple representation of a programming language module
data Module = Module
    { moduleName :: String
    , dependencies :: [String]
    , sourceCode :: String
    } deriving (Show, Eq)

-- | Simulating a module compilation process
compileModule :: (MonadIO m, Monad m) => Operations Key Identity m -> Key String -> m String
compileModule Operations{fetch, fetches} (ModuleKey name) = do
    Identity modules <- fetch ModuleGraphKey
    case lookup name [(moduleName m, m) | m <- modules] of
        Just ms -> do
            let keys = map ModuleKey (dependencies ms)
            _ <- fetches keys
            liftIO $ threadDelay 100_000
            return $ "Compiled: " ++ name
        Nothing -> error $ "Module " ++ name ++ " not found"

-- | Rule for computing the module graph
discoverModuleGraph :: (Monad m) => Operations Key Identity m -> Key [Module] -> m [Module]
discoverModuleGraph _ _ = do
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

-- | Print the module dependency tree
printModuleTree :: (Monad m, MonadIO m) => Operations Key Identity m -> [Module] -> m ()
printModuleTree ops modules = do
    liftIO $ putStrLn "Module Dependency Tree:"
    -- Find the root modules (those that have no dependents)
    let rootModules = filter (\m -> moduleName m == "Root") modules
    forM_ rootModules $ \root ->
        printModuleNode ops root modules 0

-- | Print a single module node and its dependencies recursively
printModuleNode :: (Monad m, MonadIO m) => Operations Key Identity m -> Module -> [Module] -> Int -> m ()
printModuleNode ops m allModules depth = do
    -- Print the current module with proper indentation
    liftIO $ putStrLn $ replicate (depth * 2) ' ' ++ "- " ++ moduleName m
    -- Print each dependency
    forM_ (dependencies m) $ \depName -> do
        case lookup depName [(moduleName m', m') | m' <- allModules] of
            Just depMod -> printModuleNode ops depMod allModules (depth + 1)
            Nothing -> liftIO $ putStrLn $ replicate ((depth + 1) * 2) ' ' ++ "- " ++ depName ++ " (missing)"

-- | Example: Compiling a small program with dynamic dependencies
exampleBuild :: (Monad m, MonadIO m)
             => Operations Key Identity m
             -> m ()
exampleBuild ops@Operations{..} = do
    Identity modules <- fetch ModuleGraphKey
    -- Print the module dependency tree
    printModuleTree ops modules
    -- Compile all modules
    _ <- fetches (map (ModuleKey . moduleName) modules)

    return ()