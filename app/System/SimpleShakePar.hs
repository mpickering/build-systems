{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.SimpleShakePar (
    Build,
    BuildState(..),
    build,
    printBuildStats,
    evalBuildWithThreads
) where

import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.GADT.Show
import qualified Data.Map as Map
import Data.List (sortBy, nub)
import Control.Monad (forM_)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Foldable (traverse_)
import Control.Monad (forM)
import Control.Monad.Reader
import Abstract.Operations
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)

-- | Information about a task execution
data TaskExecution = TaskExecution
    { taskKey :: String           -- The key that was built
    , workerThreadId :: Int         -- Which worker built it
    , startTime :: UTCTime        -- When the task started
    , endTime :: UTCTime          -- When the task completed
    , duration :: NominalDiffTime -- How long it took
    }

-- | The Build state
data BuildState k = BuildState
    { results :: DMap.DMap k Identity        -- Cache of computed results
    , accessCounts :: Map.Map String Int     -- Track number of times each key is accessed
    , waitCounts :: Map.Map String Int       -- Track number of times we waited for a pending task
    , buildQueue :: TChan (BuildTask k)      -- Queue for tasks
    , pendingTasks :: DMap.DMap k TMVar      -- Tasks currently in progress
    , taskExecutions :: [TaskExecution]      -- Record of task executions with timing
    }

-- | A task to be built
data BuildTask k where
    BuildTask :: k a -> TMVar a -> BuildTask k

-- | The Build monad
newtype Build k a = Build { runBuild :: ReaderT (TVar (BuildState k)) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | MonadFail instance for the Build monad
instance MonadFail (Build k) where
    fail msg = Build $ lift $ fail msg

-- | Get the current build state
getBuildState :: Build k (BuildState k)
getBuildState = Build $ do
    stVar <- ask
    lift $ readTVarIO stVar

-- | Modify the build state
modifyBuildState :: (BuildState k -> BuildState k) -> Build k ()
modifyBuildState f = Build $ do
    stVar <- ask
    lift $ atomically $ modifyTVar' stVar f

-- | Increment the access count for a key
incrementAccessCount :: GShow k => k a -> Build k ()
incrementAccessCount key = modifyBuildState $ \s ->
    let keyStr = gshow key
        newCount = Map.findWithDefault 0 keyStr (accessCounts s) + 1
    in s { accessCounts = Map.insert keyStr newCount (accessCounts s) }

-- | Initialize the access count for a key to zero
initAccessCount :: GShow k => k a -> Build k ()
initAccessCount key = modifyBuildState $ \s ->
    let keyStr = gshow key
    in s { accessCounts = Map.insertWith (\_ old -> old) keyStr 0 (accessCounts s) }


-- | Increment the wait count for a key
incrementWaitCount :: GShow k => k a -> Build k ()
incrementWaitCount key = modifyBuildState $ \s ->
    let keyStr = gshow key
        newCount = Map.findWithDefault 0 keyStr (waitCounts s) + 1
    in s { waitCounts = Map.insert keyStr newCount (waitCounts s) }

-- | Add a pending task
addPendingTask :: GCompare k => k a -> TMVar a -> Build k ()
addPendingTask key completionVar = modifyBuildState $ \s ->
    s { pendingTasks = DMap.insert key completionVar (pendingTasks s) }

-- | Check if a key has already been computed
-- Returns Just value if computed, Nothing if not yet computed
lookupComputed :: GCompare k => k a -> Build k (Maybe a)
lookupComputed key = do
    st <- getBuildState
    return $ case DMap.lookup key (results st) of
        Just (Identity res) -> Just res
        Nothing -> Nothing

-- | Check if a key is currently being computed
-- Returns Just completion var if pending, Nothing if not pending
lookupPending :: GCompare k => k a -> Build k (Maybe (TMVar a))
lookupPending key = do
    st <- getBuildState
    return $ DMap.lookup key (pendingTasks st)

-- | Submit a build task for a key
submitBuild :: (GCompare k, GShow k) => TChan (BuildTask k) -> k a -> Build k (Either a (TMVar a))
submitBuild taskQueue key = do
    is_computed <- lookupComputed key
    initAccessCount key
    case is_computed of
        Just res -> do
            -- Already computed, return the result directly
            incrementAccessCount key
            return (Left res)
        Nothing -> do
            -- Check if this key is already being computed
            pending <- lookupPending key
            case pending of
                Just resultVar -> do
                    -- Increment wait counter for this key
                    incrementWaitCount key
                    -- Already pending, return the completion var
                    return (Right resultVar)
                Nothing -> do
                    liftIO $ putStrLn $ "Building: " ++ gshow key
                    -- Create result variable
                    resultVar <- liftIO $ newEmptyTMVarIO

                    -- Register as pending
                    addPendingTask key resultVar
                    -- Submit task to queue
                    liftIO $ atomically $ writeTChan taskQueue (BuildTask key resultVar)

                    return (Right resultVar)

-- | Wait for a pending task to complete
waitForPending :: (GCompare k, GShow k) => k a -> TMVar a -> Build k a
waitForPending key resultVar = do
    -- Wait for the task to complete
    liftIO $ putStrLn $ "Waiting for: " ++ gshow key
    res <- liftIO $ atomically $ readTMVar resultVar
    liftIO $ putStrLn $ "Got: " ++ gshow key
    return res

-- | Build a value for a key, using the provided function to compute it if needed
build :: (GCompare k, GShow k) => TChan (BuildTask k) -> k a -> Build k a
build taskQueue key = do
    buildResult <- submitBuild taskQueue key
    case buildResult of
        Left res -> return res  -- Already computed, return directly
        Right resultVar -> waitForPending key resultVar

-- | Build multiple values with the same type in parallel
-- This function submits all tasks first and then waits for all results
buildMany :: (GCompare k, GShow k) => TChan (BuildTask k) -> [k a] -> Build k [a]
buildMany taskQueue keys = do
    liftIO $ putStrLn $ "Submitting " ++ show (length keys) ++ " tasks"
    -- First submit all build tasks
    buildResults <- mapM (submitBuild taskQueue) keys

    -- Then wait for all results
    forM (zip keys buildResults) $ \(key, result) -> case result of
        Left res -> return res  -- Already computed
        Right resultVar -> waitForPending key resultVar

-- | Worker function that processes build tasks
workerThread :: (GCompare k, GShow k) => Int ->
                (forall a . k a -> Build k a) ->
                TVar (BuildState k) ->
                TChan (BuildTask k) ->
                TMVar () ->
                IO ()
workerThread n compute stateVar taskQueue terminateVar = loop
  where
    loop = do
        mtask <- atomically $ (Just <$> readTChan taskQueue) `orElse`
                              (Nothing <$ readTMVar terminateVar)
        case mtask of
            Nothing -> return () -- Termination signal received
            Just (BuildTask key resultVar) -> do
                liftIO $ putStrLn $ "Worker " ++ show n ++ " processing: " ++ gshow key

                -- Record start time
                taskStartTime <- getCurrentTime

                -- Execute the task
                result <- try $ runReaderT (runBuild (compute key)) stateVar

                -- Record end time
                taskEndTime <- getCurrentTime
                let taskDuration = diffUTCTime taskEndTime taskStartTime

                case result of
                    Left (e :: SomeException) -> do
                        -- In case of error, we still need to fill the result var to avoid deadlock
                        -- but we'll propagate the exception
                        putStrLn $ "Error building " ++ gshow key ++ ": " ++ show e
                        error $ "Build failed: " ++ show e
                    Right res -> do
                        -- Update the shared state
                        putStrLn $ "Completed: " ++ gshow key ++ " by worker " ++ show n ++
                                  " in " ++ show taskDuration

                        -- Create execution record
                        let execution = TaskExecution
                                { taskKey = gshow key
                                , workerThreadId = n
                                , startTime = taskStartTime
                                , endTime = taskEndTime
                                , duration = taskDuration
                                }

                        atomically $ do
                            modifyTVar' stateVar $ \s -> s
                                { results = DMap.insert key (Identity res) (results s)
                                , pendingTasks = DMap.delete key (pendingTasks s)
                                , taskExecutions = execution : taskExecutions s
                                }
                            -- Put the result in the result var
                            putTMVar resultVar res
                        loop

-- | Print task execution statistics
printTaskExecutionStats :: Build k ()
printTaskExecutionStats = do
    st <- getBuildState
    liftIO $ putStrLn "\nTask Execution Statistics:"
    liftIO $ putStrLn "========================="

    let executions = sortBy (\a b -> compare (startTime a) (startTime b)) (taskExecutions st)

    if null executions
        then liftIO $ putStrLn "No tasks were executed."
        else do
            -- Print summary per worker
            let byWorker = Map.fromListWith (++)
                         $ map (\e -> (workerThreadId e, [e])) executions

            forM_ (Map.toList byWorker) $ \(worker, execs) -> do
                let totalTime = sum $ map duration execs
                    taskCount = length execs
                liftIO $ putStrLn $ "Worker " ++ show worker ++
                                  ": " ++ show taskCount ++ " tasks, " ++
                                  "total time: " ++ show totalTime

            -- Print all task executions in time order
            liftIO $ putStrLn "\nIndividual Task Executions:"
            forM_ executions $ \exec -> do
                liftIO $ putStrLn $ "Task: " ++ taskKey exec ++
                                  ", Worker: " ++ show (workerThreadId exec) ++
                                  ", Duration: " ++ show (duration exec)

-- | Print build statistics
printBuildStats :: Build k ()
printBuildStats = do
    st <- getBuildState
    liftIO $ putStrLn "\nBuild Cache Statistics:"
    liftIO $ putStrLn "========================"

    let accessStats = Map.toList (accessCounts st)
    let waitStats = Map.toList (waitCounts st)
    let allKeys = nub $ map fst accessStats ++ map fst waitStats

    if null allKeys
        then liftIO $ putStrLn "No cached results were accessed."
        else forM_ (sortByKey allKeys) $ \key -> do
            let accessCount = Map.findWithDefault 0 key (accessCounts st)
            let waitCount = Map.findWithDefault 0 key (waitCounts st)
            liftIO $ putStrLn $ key ++ ": accessed " ++ show accessCount ++
                                " time(s), waited " ++ show waitCount ++ " time(s)"

    -- Also print task execution statistics
    printTaskExecutionStats
  where
    sortByKey = sortBy (\k1 k2 -> compare k1 k2)

-- | Evaluate a build action with parallel execution
evalBuildWithThreads :: forall k a .(GShow k, GCompare k) => Int
                     -> (forall z . Operations k (Build k) -> k z -> Build k z)
                     -> (Operations k (Build k) -> Build k a)
                     -> IO a
evalBuildWithThreads numThreads compute_func buildAction = do
        -- Create task queue
        taskQueue <- newTChanIO

        -- Create state with task queue
        let initialState = BuildState DMap.empty Map.empty Map.empty taskQueue DMap.empty []

        -- Create shared state variable
        stateVar <- newTVarIO initialState

        -- Create termination signal
        terminateVar <- newEmptyTMVarIO

        let queueOps = Operations {
                        fetch = build taskQueue
                      , fetches = buildMany taskQueue
                      }
        let compute :: forall z . k z -> Build k z
            compute = compute_func queueOps


        -- Start worker threads
        workers <- forM [0..numThreads-1] $ \i ->
            async $ workerThread i compute stateVar taskQueue terminateVar

        result <-
            catch
                (runReaderT (runBuild (buildAction queueOps)) stateVar)
                (\(e :: SomeException) -> do
                    -- Signal workers to terminate in case of error
                    atomically $ putTMVar terminateVar ()
                    -- Cancel all workers
                    traverse_ cancel workers
                    -- Re-throw the exception
                    throwIO e)

        -- Signal workers to terminate
        atomically $ putTMVar terminateVar ()

        -- Wait for all workers to finish
        traverse_ wait workers

        -- Return the result
        return result