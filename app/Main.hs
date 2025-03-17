{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.State
import qualified Data.Dependent.Map as DMap
import qualified DMap2 as DMap2
import Data.Dependent.Sum (DSum(..))
import Data.GADT.Compare
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Data.Kind (Type)
import Control.Monad.Identity
import Data.Type.Equality
import qualified Data.Map as Map
import Data.List (sortBy)

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


data RuleResult o  = RuleResult { getRuleResult :: Key o -> Build o }

-- | The Build monad, now storing rules as a dependent map
data BuildState = BuildState
    { rules :: DMap.DMap RuleMatch RuleResult -- Dependent map for rules
    , results :: DMap.DMap Key Identity
    , accessCounts :: Map.Map String Int      -- Track number of times each key is accessed
    }

newtype Build a = Build { runBuild :: StateT BuildState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState BuildState)

-- | MonadFail instance for the Build monad
instance MonadFail Build where
    fail msg = Build $ lift $ fail msg


-- | Define a rule with a matching pattern
rule :: RuleMatch o -> (Key o -> Build o) -> Build ()
rule match action = modify $ \st ->
    st { rules = DMap.insert match (RuleResult action) (rules st) }

-- | Find a rule matching a given key
findRule :: Key o -> Build (Maybe (Key o -> Build o))
findRule key = do
    st <- get
    return $ fmap getRuleResult (DMap.lookup (keyToMatch key) (rules st))

-- | Map a Key to its corresponding RuleMatch
keyToMatch :: Key a -> (RuleMatch a)
keyToMatch (ModuleKey _) = MatchModule
keyToMatch ModuleGraphKey = MatchModuleGraph


-- | Check if a result exists and execute the rule if needed
build :: Key a -> Build (Maybe a)
build key = do
    st <- get
    case DMap.lookup key (results st) of
        Just (Identity res) -> do
            -- Increment access counter for this key
            let keyStr = keyToString key
                newCount = Map.findWithDefault 0 keyStr (accessCounts st) + 1
            modify $ \s -> s { accessCounts = Map.insert keyStr newCount (accessCounts s) }
            return (Just res)
        Nothing -> do
            ruleOpt <- findRule key
            case ruleOpt of
                Just action -> do
                    res <- action key
                    modify $ \st' -> st' { results = DMap.insert key (Identity res) (results st') }
                    return (Just res)
                Nothing -> return Nothing

-- | Convert a key to its string representation for stats tracking
keyToString :: Key a -> String
keyToString (ModuleKey name) = "Module:" ++ name
keyToString ModuleGraphKey = "ModuleGraph"

-- | Print build statistics
printBuildStats :: Build ()
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

-- | A simple representation of a programming language module
data Module = Module
    { moduleName :: String
    , dependencies :: [String]
    , sourceCode :: String
    } deriving (Show, Eq)

-- | Simulating a module compilation process
compileModule :: Key String -> Build String
compileModule (ModuleKey name) = do
    liftIO $ putStrLn $ "Compiling module: " ++ name
    Just modules <- build ModuleGraphKey
    case lookup name [(moduleName m, m) | m <- modules] of
        Just ms -> do
            forM_ (dependencies ms) (\dep -> build (ModuleKey dep)) -- Query dependencies first
            return $ "Compiled: " ++ name
        Nothing -> error $ "Module " ++ name ++ " not found"

-- | Rule for computing the module graph
discoverModuleGraph :: Key [Module] -> Build [Module]
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
printModuleTree :: [Module] -> Build ()
printModuleTree modules = do
    liftIO $ putStrLn "Module Dependency Tree:"
    -- Find the root modules (those that have no dependents)
    let rootModules = filter (\m -> moduleName m == "Root") modules
    forM_ rootModules $ \root ->
        printModuleNode root modules 0

-- | Print a single module node and its dependencies recursively
printModuleNode :: Module -> [Module] -> Int -> Build ()
printModuleNode m allModules depth = do
    -- Print the current module with proper indentation
    liftIO $ putStrLn $ replicate (depth * 2) ' ' ++ "- " ++ moduleName m

    -- Print each dependency
    forM_ (dependencies m) $ \depName -> do
        case lookup depName [(moduleName m, m) | m <- allModules] of
            Just depMod -> printModuleNode depMod allModules (depth + 1)
            Nothing -> liftIO $ putStrLn $ replicate ((depth + 1) * 2) ' ' ++ "- " ++ depName ++ " (missing)"

-- | Example: Compiling a small program with dynamic dependencies
exampleBuild :: Build ()
exampleBuild = do
    rule MatchModuleGraph discoverModuleGraph
    rule MatchModule compileModule -- Parameterized rule for compiling any module

    Just modules <- build ModuleGraphKey

    -- Print the module dependency tree
    printModuleTree modules

    -- Compile all modules
    forM_ modules $ \ms -> do
        _ <- build (ModuleKey (moduleName ms))
        return ()

    -- Print build statistics
    printBuildStats

    return ()

main :: IO ()
main = evalStateT (runBuild exampleBuild) (BuildState DMap.empty DMap.empty Map.empty)

