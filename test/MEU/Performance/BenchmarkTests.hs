{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MEU.Performance.BenchmarkTests
  ( -- * Benchmark Test Suite
    runComprehensiveBenchmarks
  , benchmarkMonadicStacking
  , benchmarkFeedbackMechanisms
  , benchmarkMEUSystemScaling

    -- * Performance Metrics
  , PerformanceMetrics(..)
  , BenchmarkResult(..)
  , ScalingTestResult(..)

    -- * Test Configuration
  , BenchmarkConfig(..)
  , TestDepth(..)
  , TestScale(..)
  ) where

import Control.Monad (forM, forM_, replicateM)
import Control.Concurrent (threadDelay)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Effectful
import Effectful.State.Static.Local
import Effectful.Error.Static
import Effectful.Reader.Static

import MEU.Core.Types
import MEU.WS.StateTracker (StateTracker, createDefaultStateTracker)
import MEU.WS.FeedbackMechanism

-- ============================================================================
-- Benchmark Configuration and Types
-- ============================================================================

-- | Test depth configuration
data TestDepth = TestDepth
  { depthLevels :: Int -- 2-10 levels
  , tripletsPerLevel :: Int -- Number of triplets at each level
  } deriving (Eq, Show, Generic)

-- | Test scale configuration
data TestScale = TestScale
  { totalTriplets :: Int -- 20-1000 triplets
  , operationsPerTriplet :: Int -- Operations to perform per triplet
  , feedbackRatio :: Double -- Ratio of operations that generate feedback
  } deriving (Eq, Show, Generic)

-- | Benchmark configuration
data BenchmarkConfig = BenchmarkConfig
  { testDepths :: [TestDepth]
  , testScales :: [TestScale]
  , iterations :: Int -- Number of times to run each test
  , warmupIterations :: Int -- Warmup runs before measuring
  } deriving (Eq, Show, Generic)

-- | Performance metrics
data PerformanceMetrics = PerformanceMetrics
  { pmCPUTime :: Double -- CPU time in seconds
  , pmWallTime :: Double -- Wall clock time in seconds
  , pmMemoryUsage :: Int -- Memory usage in bytes (estimated)
  , pmOperationsPerSecond :: Double
  , pmFeedbackLatency :: Double -- Average feedback processing time
  , pmStackingOverhead :: Double -- Monadic stacking overhead
  } deriving (Eq, Show, Generic)

-- | Result of a single benchmark
data BenchmarkResult = BenchmarkResult
  { brTestName :: Text
  , brConfig :: Text
  , brMetrics :: PerformanceMetrics
  , brSuccess :: Bool
  , brErrors :: [Text]
  } deriving (Eq, Show, Generic)

-- | Result of scaling tests
data ScalingTestResult = ScalingTestResult
  { strDepth :: Int
  , strTripletCount :: Int
  , strResults :: [BenchmarkResult]
  , strScalingFactor :: Double -- Performance degradation factor
  } deriving (Eq, Show, Generic)

-- ============================================================================
-- Main Benchmark Suite
-- ============================================================================

-- | Run comprehensive benchmarks testing all aspects of the MEU system
runComprehensiveBenchmarks :: IO [ScalingTestResult]
runComprehensiveBenchmarks = do
  putStrLn "=== MEU Framework Comprehensive Benchmark Suite ==="
  putStrLn "Testing monadic stacking efficiency and feedback mechanisms"
  putStrLn ""

  let config = BenchmarkConfig
        { testDepths = [ TestDepth d (10 * d) | d <- [2..10] ]
        , testScales = [ TestScale t (t `div` 10) 0.3 | t <- [20, 50, 100, 200, 500, 1000] ]
        , iterations = 5
        , warmupIterations = 2
        }

  results <- runEff $ do
    -- Test monadic stacking performance
    stackingResults <- benchmarkMonadicStacking config

    -- Test feedback mechanism performance
    feedbackResults <- benchmarkFeedbackMechanisms config

    -- Test MEU system scaling
    scalingResults <- benchmarkMEUSystemScaling config

    return $ stackingResults ++ feedbackResults ++ scalingResults

  putStrLn "\n=== Benchmark Results Summary ==="
  forM_ results $ \result -> do
    putStrLn $ printf "Depth: %d, Triplets: %d, Scaling Factor: %.2f"
      (strDepth result) (strTripletCount result) (strScalingFactor result)

  return results

-- ============================================================================
-- Monadic Stacking Benchmarks
-- ============================================================================

-- | Benchmark monadic stacking efficiency with various depths
benchmarkMonadicStacking ::
  (IOE :> es) =>
  BenchmarkConfig ->
  Eff es [ScalingTestResult]
benchmarkMonadicStacking config = do
  liftIO $ putStrLn "Running monadic stacking benchmarks..."

  results <- forM (testDepths config) $ \depth -> do
    liftIO $ putStrLn $ printf "Testing depth %d with %d triplets per level"
      (depthLevels depth) (tripletsPerLevel depth)

    benchResults <- forM [1..iterations config] $ \iteration -> do
      -- Warmup if needed
      when (iteration <= warmupIterations config) $ do
        _ <- runStackingTest depth
        return ()

      -- Actual measurement
      if iteration > warmupIterations config
        then do
          startTime <- liftIO getCurrentTime
          startCPU <- liftIO getCPUTime

          result <- runStackingTest depth

          endCPU <- liftIO getCPUTime
          endTime <- liftIO getCurrentTime

          let cpuTime = fromIntegral (endCPU - startCPU) / (10^12)
          let wallTime = realToFrac $ diffUTCTime endTime startTime
          let totalTriplets = depthLevels depth * tripletsPerLevel depth

          return $ Just $ BenchmarkResult
            { brTestName = "monadic_stacking"
            , brConfig = T.pack $ show depth
            , brMetrics = PerformanceMetrics
              { pmCPUTime = cpuTime
              , pmWallTime = wallTime
              , pmMemoryUsage = estimateMemoryUsage totalTriplets
              , pmOperationsPerSecond = fromIntegral totalTriplets / wallTime
              , pmFeedbackLatency = 0.0
              , pmStackingOverhead = cpuTime / fromIntegral totalTriplets
              }
            , brSuccess = True
            , brErrors = []
            }
        else return Nothing

    let validResults = [r | Just r <- benchResults]
    let avgMetrics = averageMetrics validResults
    let scalingFactor = calculateScalingFactor validResults

    return $ ScalingTestResult
      { strDepth = depthLevels depth
      , strTripletCount = depthLevels depth * tripletsPerLevel depth
      , strResults = validResults
      , strScalingFactor = scalingFactor
      }

  return results

-- | Run a single stacking test with specified depth
runStackingTest ::
  (IOE :> es) =>
  TestDepth ->
  Eff es Bool
runStackingTest depth = do
  -- Create a hierarchy of MEU triplets with specified depth
  rootTriplet <- createTestTriplet 0 (TripletId UUID.nil)

  -- Create nested triplets
  _ <- foldM (\parentId level -> do
    levelTriplets <- forM [1..tripletsPerLevel depth] $ \i -> do
      tripletId <- liftIO UUID.nextRandom
      let triplet = createTestTriplet level (TripletId tripletId)
      -- Simulate monadic stacking operations
      _ <- performStackingOperations triplet level
      return (TripletId tripletId)
    return $ head levelTriplets
  ) (TripletId UUID.nil) [1..depthLevels depth]

  return True

-- ============================================================================
-- Feedback Mechanism Benchmarks
-- ============================================================================

-- | Benchmark feedback mechanism performance (I*, O*, R* operations)
benchmarkFeedbackMechanisms ::
  (IOE :> es) =>
  BenchmarkConfig ->
  Eff es [ScalingTestResult]
benchmarkFeedbackMechanisms config = do
  liftIO $ putStrLn "Running feedback mechanism benchmarks..."

  results <- forM (testScales config) $ \scale -> do
    liftIO $ putStrLn $ printf "Testing %d triplets with %d operations each"
      (totalTriplets scale) (operationsPerTriplet scale)

    benchResults <- forM [1..iterations config] $ \iteration -> do
      if iteration > warmupIterations config
        then do
          startTime <- liftIO getCurrentTime
          startCPU <- liftIO getCPUTime

          result <- runFeedbackTest scale

          endCPU <- liftIO getCPUTime
          endTime <- liftIO getCurrentTime

          let cpuTime = fromIntegral (endCPU - startCPU) / (10^12)
          let wallTime = realToFrac $ diffUTCTime endTime startTime
          let totalOps = totalTriplets scale * operationsPerTriplet scale

          return $ Just $ BenchmarkResult
            { brTestName = "feedback_mechanisms"
            , brConfig = T.pack $ show scale
            , brMetrics = PerformanceMetrics
              { pmCPUTime = cpuTime
              , pmWallTime = wallTime
              , pmMemoryUsage = estimateMemoryUsage (totalTriplets scale)
              , pmOperationsPerSecond = fromIntegral totalOps / wallTime
              , pmFeedbackLatency = cpuTime / fromIntegral totalOps
              , pmStackingOverhead = 0.0
              }
            , brSuccess = True
            , brErrors = []
            }
        else do
          _ <- runFeedbackTest scale
          return Nothing

    let validResults = [r | Just r <- benchResults]
    let scalingFactor = calculateScalingFactor validResults

    return $ ScalingTestResult
      { strDepth = 1
      , strTripletCount = totalTriplets scale
      , strResults = validResults
      , strScalingFactor = scalingFactor
      }

  return results

-- | Run feedback mechanism test
runFeedbackTest ::
  (IOE :> es) =>
  TestScale ->
  Eff es Bool
runFeedbackTest scale = runErrorNoCallStack @MEUError $ evalState (undefined :: StateTracker) $ do
  -- Create test triplets
  triplets <- forM [1..totalTriplets scale] $ \i -> do
    tripletId <- liftIO UUID.nextRandom
    return $ createTestTriplet 1 (TripletId tripletId)

  -- Generate and process feedback signals
  forM_ triplets $ \triplet -> do
    forM_ [1..operationsPerTriplet scale] $ \opNum -> do
      -- Create mock feedback signal
      feedback <- generateTestFeedback triplet opNum

      -- Process feedback through right adjoint mechanisms
      _ <- processFeedbackSignal feedback triplet

      -- Test specific right adjoint operations
      _ <- executeRightAdjointUpdate InstantiationStar feedback triplet
      _ <- executeRightAdjointUpdate OutputStar feedback triplet
      _ <- executeRightAdjointUpdate ResponseStar feedback triplet

      return ()

  return True

-- ============================================================================
-- MEU System Scaling Benchmarks
-- ============================================================================

-- | Benchmark overall MEU system scaling with various configurations
benchmarkMEUSystemScaling ::
  (IOE :> es) =>
  BenchmarkConfig ->
  Eff es [ScalingTestResult]
benchmarkMEUSystemScaling config = do
  liftIO $ putStrLn "Running MEU system scaling benchmarks..."

  -- Test combined depth and scale scenarios
  let combinedTests = [(d, s) | d <- take 5 (testDepths config), s <- take 3 (testScales config)]

  results <- forM combinedTests $ \(depth, scale) -> do
    let totalTripletsCount = depthLevels depth * tripletsPerLevel depth + totalTriplets scale

    liftIO $ putStrLn $ printf "Testing depth %d + scale %d (total: %d triplets)"
      (depthLevels depth) (totalTriplets scale) totalTripletsCount

    benchResults <- forM [1..min 3 (iterations config)] $ \iteration -> do
      if iteration > 1 -- Reduced warmup for combined tests
        then do
          startTime <- liftIO getCurrentTime
          startCPU <- liftIO getCPUTime

          result <- runCombinedTest depth scale

          endCPU <- liftIO getCPUTime
          endTime <- liftIO getCurrentTime

          let cpuTime = fromIntegral (endCPU - startCPU) / (10^12)
          let wallTime = realToFrac $ diffUTCTime endTime startTime

          return $ Just $ BenchmarkResult
            { brTestName = "meu_system_scaling"
            , brConfig = T.pack $ printf "depth_%d_scale_%d" (depthLevels depth) (totalTriplets scale)
            , brMetrics = PerformanceMetrics
              { pmCPUTime = cpuTime
              , pmWallTime = wallTime
              , pmMemoryUsage = estimateMemoryUsage totalTripletsCount
              , pmOperationsPerSecond = fromIntegral totalTripletsCount / wallTime
              , pmFeedbackLatency = cpuTime / fromIntegral totalTripletsCount
              , pmStackingOverhead = cpuTime / fromIntegral (depthLevels depth)
              }
            , brSuccess = True
            , brErrors = []
            }
        else do
          _ <- runCombinedTest depth scale
          return Nothing

    let validResults = [r | Just r <- benchResults]
    let scalingFactor = calculateScalingFactor validResults

    return $ ScalingTestResult
      { strDepth = depthLevels depth
      , strTripletCount = totalTripletsCount
      , strResults = validResults
      , strScalingFactor = scalingFactor
      }

  return results

-- | Run combined depth and scale test
runCombinedTest ::
  (IOE :> es) =>
  TestDepth ->
  TestScale ->
  Eff es Bool
runCombinedTest depth scale = runErrorNoCallStack @MEUError $ evalState (undefined :: StateTracker) $ do
  -- Run both stacking and feedback tests
  stackingResult <- lift $ runStackingTest depth
  feedbackResult <- runFeedbackTest scale
  return (stackingResult && feedbackResult)

-- ============================================================================
-- Helper Functions and Test Utilities
-- ============================================================================

-- | Create a test MEU triplet for benchmarking
createTestTriplet ::
  (IOE :> es) =>
  Int ->
  TripletId ->
  MEUTripletEff Instantiated Validated Executable es
createTestTriplet level parentId =
  LeafTripletEff
    { leafTripletId = parentId
    , leafParentId = parentId
    , leafAncestors = replicate level parentId
    , leafModelDomain = createTestModelDomain level
    , leafExecuteDomain = createTestExecuteDomain level
    , leafUpdateDomain = createTestUpdateDomain level
    , leafDataflowArrows = createTestDataflowArrows
    , leafSUDPointers = Map.empty
    , leafExecutionResults = Map.empty
    , leafGeometricTheory = GeometricTheory Set.empty Set.empty Set.empty Set.empty Set.empty True
    , leafMetadata = createTestMetadata level
    }

-- | Create test model domain state
createTestModelDomain :: Int -> DomainState 'ModelDomain Instantiated es
createTestModelDomain level = ModelDomainState
  { modelDomainSpecs = Map.fromList [("level", TypedValue IntType (IntValue $ fromIntegral level))]
  , modelDomainDSLOps = Map.empty
  , modelDomainTypes = Set.singleton (BaseType IntType)
  , modelDomainInheritanceChain = [TripletId UUID.nil]
  }

-- | Create test execute domain state
createTestExecuteDomain :: Int -> DomainState 'ExecuteDomain Executable es
createTestExecuteDomain level = ExecuteDomainState
  { executeDomainEnvironment = ExecutionEnvironment ("test-env-" <> T.pack (show level)) "Test environment" "{}"
  , executeDomainDeployedOps = Map.empty
  , executeDomainResourceAlloc = ResourceAllocation level (1024 * level) (30000 * level)
  , executeDomainInheritanceChain = [TripletId UUID.nil]
  }

-- | Create test update domain state
createTestUpdateDomain :: Int -> DomainState 'UpdateDomain Validated es
createTestUpdateDomain level = UpdateDomainState
  { updateDomainVerifiers = Map.empty
  , updateDomainGeometric = GeometricTheory Set.empty Set.empty Set.empty Set.empty Set.empty True
  , updateDomainCriteria = []
  , updateDomainInheritanceChain = [TripletId UUID.nil]
  }

-- | Create test dataflow arrows
createTestDataflowArrows :: DataflowArrowCollection es
createTestDataflowArrows = DataflowArrowCollection
  { arrowME = createTestAdjointPair
  , arrowEM = createTestAdjointPair
  , arrowEU = createTestAdjointPair
  , arrowUE = createTestAdjointPair
  , arrowUM = createTestAdjointPair
  , arrowMU = createTestAdjointPair
  }

-- | Create test adjoint pair
createTestAdjointPair :: AdjointPair source target es
createTestAdjointPair = AdjointPair
  { leftAdjoint = \domain -> return domain
  , rightAdjoint = \domain -> return domain
  , unit = \_ -> return undefined
  , counit = \_ -> return ()
  , identityMap = \x -> return x
  }

-- | Create test metadata
createTestMetadata :: Int -> TripletMetadata
createTestMetadata level = TripletMetadata
  { tripletCreated = Timestamp $ read "2024-01-01 00:00:00 UTC"
  , tripletUpdated = Timestamp $ read "2024-01-01 00:00:00 UTC"
  , tripletVersion = Version 0 1 0
  , tripletDescription = T.pack $ "Test triplet level " ++ show level
  , tripletParentId = Nothing
  , tripletChildIds = []
  , tripletTags = ["test", "benchmark"]
  , tripletDepth = level
  }

-- | Perform monadic stacking operations for benchmarking
performStackingOperations ::
  (IOE :> es) =>
  MEUTripletEff Instantiated Validated Executable es ->
  Int ->
  Eff es ()
performStackingOperations triplet level = do
  -- Simulate various monadic operations that test stacking efficiency
  forM_ [1..level * 10] $ \i -> do
    -- Simulate domain transitions that test the monadic stack
    _ <- return $ i `mod` 3 -- Simple operation
    return ()

-- | Generate test feedback signal
generateTestFeedback ::
  (IOE :> es) =>
  MEUTripletEff inst val exec es ->
  Int ->
  Eff es FeedbackSignal
generateTestFeedback triplet opNum = do
  feedbackId <- liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime

  return $ FeedbackSignal
    { feedbackId = feedbackId
    , feedbackType = if opNum `mod` 2 == 0 then ExecutionSuccess else PerformanceMetrics
    , feedbackSourceTriplet = getTripletId triplet
    , feedbackSourceDomain = 'ExecuteDomain
    , feedbackTimestamp = timestamp
    , feedbackData = Map.fromList
      [ ("operation_number", TypedValue IntType (IntValue $ fromIntegral opNum))
      , ("execution_time", TypedValue FloatType (FloatValue $ fromIntegral opNum * 1.5))
      ]
    , feedbackOriginalRequest = Just $ "test_operation_" <> T.pack (show opNum)
    , feedbackExecutionTime = fromIntegral opNum * 1.5
    }

-- | Get triplet ID from GADT
getTripletId :: MEUTripletEff inst val exec es -> TripletId
getTripletId (SourceTripletEff{sourceTripletId = tid}) = tid
getTripletId (BranchTripletEff{branchTripletId = tid}) = tid
getTripletId (LeafTripletEff{leafTripletId = tid}) = tid

-- | Estimate memory usage for given number of triplets
estimateMemoryUsage :: Int -> Int
estimateMemoryUsage tripletCount = tripletCount * 2048 -- Rough estimate in bytes

-- | Calculate average metrics from benchmark results
averageMetrics :: [BenchmarkResult] -> PerformanceMetrics
averageMetrics results =
  let count = fromIntegral $ length results
      sumMetrics = foldl addMetrics zeroMetrics (map brMetrics results)
  in divideMetrics sumMetrics count
  where
    zeroMetrics = PerformanceMetrics 0 0 0 0 0 0
    addMetrics (PerformanceMetrics a1 b1 c1 d1 e1 f1) (PerformanceMetrics a2 b2 c2 d2 e2 f2) =
      PerformanceMetrics (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2) (f1+f2)
    divideMetrics (PerformanceMetrics a b c d e f) n =
      PerformanceMetrics (a/n) (b/n) (fromIntegral $ round $ fromIntegral c / n) (d/n) (e/n) (f/n)

-- | Calculate scaling factor (performance degradation)
calculateScalingFactor :: [BenchmarkResult] -> Double
calculateScalingFactor [] = 1.0
calculateScalingFactor results =
  let metrics = map brMetrics results
      wallTimes = map pmWallTime metrics
      maxTime = maximum wallTimes
      minTime = minimum wallTimes
  in if minTime > 0 then maxTime / minTime else 1.0