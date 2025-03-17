{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleBuildExample where

import Control.Monad.State
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Control.Monad (forM_)
import Data.Type.Equality
import qualified Data.Map as Map
import Build (Build(..), rule, build, printBuildStats)

type ModuleName = String

-- | A key type for dependent map
data Key a where
    ModuleKey :: ModuleName -> Key FilePath
    ModuleGraphKey :: Key [Module] -- Rule type for computing the module graph

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
data RuleMatch o where
    MatchModule :: RuleMatch FilePath
    MatchModuleGraph :: RuleMatch [Module]

instance GEq RuleMatch where
    geq MatchModule MatchModule = Just Refl
    geq MatchModuleGraph MatchModuleGraph = Just Refl
    geq _ _ = Nothing

instance GCompare RuleMatch where
    gcompare MatchModule MatchModule = GEQ
    gcompare MatchModuleGraph MatchModuleGraph = GEQ
    gcompare MatchModule MatchModuleGraph = GLT
    gcompare MatchModuleGraph MatchModule = GGT

-- | Map a Key to its corresponding RuleMatch
keyToMatch :: Key a -> RuleMatch a
keyToMatch (ModuleKey _) = MatchModule
keyToMatch ModuleGraphKey = MatchModuleGraph

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
compileModule :: Key String -> Build RuleMatch Key String
compileModule (ModuleKey name) = do
    liftIO $ putStrLn $ "Compiling module: " ++ name
    Just modules <- build keyToString keyToMatch ModuleGraphKey
    case lookup name [(moduleName m, m) | m <- modules] of
        Just ms -> do
            forM_ (dependencies ms) (\dep -> build keyToString keyToMatch (ModuleKey dep)) -- Query dependencies first
            return $ "Compiled: " ++ name
        Nothing -> error $ "Module " ++ name ++ " not found"

-- | Rule for computing the module graph
discoverModuleGraph :: Key [Module] -> Build RuleMatch Key [Module]
discoverModuleGraph _ = do
    liftIO $ putStrLn "Discovering module graph..."
    let modules = [ Module "A" [] "print(\"Hello from A\")"
                  , Module "B" ["A"] "print(\"Hello from B\")"
                  , Module "C" ["A", "B"] "print(\"Hello from C\")"
                  , Module "D" ["B"] "print(\"Hello from D\")"
                  , Module "E" ["B", "C"] "print(\"Hello from E\")"
                  , Module "F" ["C"] "print(\"Hello from F\")"
                  , Module "G" ["D", "F"] "print(\"Hello from G\")"
                  , Module "H" ["E"] "print(\"Hello from H\")"
                  , Module "I" ["G", "H"] "print(\"Hello from I\")"
                  , Module "Root" ["I"] "print(\"Hello from Root\")"
                  ]
    return modules

-- | Print the module dependency tree
printModuleTree :: [Module] -> Build RuleMatch Key ()
printModuleTree modules = do
    liftIO $ putStrLn "Module Dependency Tree:"
    -- Find the root modules (those that have no dependents)
    let rootModules = filter (\m -> moduleName m == "Root") modules
    forM_ rootModules $ \root ->
        printModuleNode root modules 0

-- | Print a single module node and its dependencies recursively
printModuleNode :: Module -> [Module] -> Int -> Build RuleMatch Key ()
printModuleNode m allModules depth = do
    -- Print the current module with proper indentation
    liftIO $ putStrLn $ replicate (depth * 2) ' ' ++ "- " ++ moduleName m

    -- Print each dependency
    forM_ (dependencies m) $ \depName -> do
        case lookup depName [(moduleName m, m) | m <- allModules] of
            Just depMod -> printModuleNode depMod allModules (depth + 1)
            Nothing -> liftIO $ putStrLn $ replicate ((depth + 1) * 2) ' ' ++ "- " ++ depName ++ " (missing)"

-- | Example: Compiling a small program with dynamic dependencies
exampleBuild :: Build RuleMatch Key ()
exampleBuild = do
    rule MatchModuleGraph discoverModuleGraph
    rule MatchModule compileModule -- Parameterized rule for compiling any module

    Just modules <- build keyToString keyToMatch ModuleGraphKey

    -- Print the module dependency tree
    printModuleTree modules

    -- Compile all modules
    forM_ modules $ \ms -> do
        _ <- build keyToString keyToMatch (ModuleKey (moduleName ms))
        return ()

    -- Print build statistics
    printBuildStats

    return ()