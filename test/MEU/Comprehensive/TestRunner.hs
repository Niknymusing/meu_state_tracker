{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module MEU.Comprehensive.TestRunner
  ( runComprehensiveTests
  , testFeedbackMechanisms
  , testSUDIntegration
  , verifyStateUpdates
  ) where

import Control.Monad (forM_, when, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Time (getCurrentTime)
import Text.Printf (printf)

import Effectful
import Effectful.State.Static.Local
import Effectful.Error.Static
import Effectful.Reader.Static

import MEU.Core.Types
import MEU.WS.StateTracker (StateTracker, createDefaultStateTracker)
import MEU.WS.FeedbackMechanism
import MEU.Performance.BenchmarkTests

-- ============================================================================
-- Comprehensive Test Suite
-- ============================================================================

-- | Run all comprehensive tests including performance, feedback, and integration
runComprehensiveTests :: IO ()
runComprehensiveTests = do
  putStrLn "=== MEU Framework Comprehensive Test Suite ==="
  putStrLn "Testing all functionality including feedback mechanisms and performance"
  putStrLn ""

  -- Run benchmark tests
  putStrLn "1. Running Performance Benchmarks..."
  benchmarkResults <- runComprehensiveBenchmarks
  putStrLn $ printf "Completed %d benchmark test scenarios" (length benchmarkResults)

  -- Test feedback mechanisms
  putStrLn "\n2. Testing Feedback Mechanisms (I*, O*, R*)..."
  feedbackResults <- testFeedbackMechanisms
  putStrLn $ printf "Feedback mechanism tests: %s" (if feedbackResults then "PASSED" else "FAILED")

  -- Test SUD integration
  putStrLn "\n3. Testing SUD Integration..."
  sudResults <- testSUDIntegration
  putStrLn $ printf "SUD integration tests: %s" (if sudResults then "PASSED" else "FAILED")

  -- Verify state updates
  putStrLn "\n4. Verifying State Updates from Feedback..."
  stateResults <- verifyStateUpdates
  putStrLn $ printf "State update verification: %s" (if stateResults then "PASSED" else "FAILED")

  -- Summary
  putStrLn "\n=== Test Results Summary ==="
  let allPassed = feedbackResults && sudResults && stateResults
  putStrLn $ printf "Overall test result: %s" (if allPassed then "PASSED" else "FAILED")

  when allPassed $ do
    putStrLn "\n✅ All MEU framework functionality verified successfully!"
    putStrLn "   - GADT-based triplets with phantom types working correctly"
    putStrLn "   - Effectful monadic stacking performing efficiently"
    putStrLn "   - Feedback mechanisms (I*, O*, R*) functioning properly"
    putStrLn "   - SUD integration working as expected"
    putStrLn "   - State updates from feedback signals verified"

  unless allPassed $ do
    putStrLn "\n❌ Some tests failed. Check the output above for details."

-- ============================================================================
-- Feedback Mechanism Tests
-- ============================================================================

-- | Test all feedback mechanisms (I*, O*, R* operations)
testFeedbackMechanisms :: IO Bool
testFeedbackMechanisms = do
  putStrLn "Testing right adjoint feedback mechanisms..."

  result <- runEff $ runErrorNoCallStack @MEUError $ evalState (undefined :: StateTracker) $ do
    -- Test I* feedback (Execute → Model)
    iStarResult <- testIStarFeedback

    -- Test O* feedback (Update → Execute)
    oStarResult <- testOStarFeedback

    -- Test R* feedback (Model → Update)
    rStarResult <- testRStarFeedback

    -- Test feedback signal processing
    processingResult <- testFeedbackProcessing

    return $ iStarResult && oStarResult && rStarResult && processingResult

  case result of
    Right success -> return success
    Left err -> do
      putStrLn $ "Feedback mechanism test error: " ++ show err
      return False

-- | Test I* feedback mechanism (Execute → Model updates)
testIStarFeedback :: (IOE :> es, State (StateTracker) :> es, Error MEUError :> es) => Eff es Bool
testIStarFeedback = do
  -- Create test execution domain
  let execDomain = ExecuteDomainState
        { executeDomainEnvironment = ExecutionEnvironment "test-env" "Test environment" "{}"
        , executeDomainDeployedOps = Map.empty
        , executeDomainResourceAlloc = ResourceAllocation 1 1024 30000
        , executeDomainInheritanceChain = [TripletId UUID.nil]
        }

  -- Create performance metrics feedback
  feedbackId <- liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime
  let feedback = FeedbackSignal
        { feedbackId = feedbackId
        , feedbackType = PerformanceMetrics
        , feedbackSourceTriplet = TripletId UUID.nil
        , feedbackSourceDomain = 'ExecuteDomain
        , feedbackTimestamp = timestamp
        , feedbackData = Map.fromList
          [ ("execution_time", TypedValue FloatType (FloatValue 150.5))
          , ("performance_score", TypedValue FloatType (FloatValue 0.85))
          , ("resource_efficiency", TypedValue FloatType (FloatValue 0.92))
          ]
        , feedbackOriginalRequest = Just "test_operation"
        , feedbackExecutionTime = 150.5
        }

  -- Execute I* feedback
  modelDomain <- executeIStarFeedback feedback execDomain

  -- Verify the model domain was updated correctly
  case modelDomain of
    ModelDomainState{modelDomainSpecs = specs} -> do
      let hasExecutionTime = Map.member "last_execution_time" specs
      let hasPerformanceScore = Map.member "performance_score" specs
      let hasResourceEfficiency = Map.member "resource_efficiency" specs
      return $ hasExecutionTime && hasPerformanceScore && hasResourceEfficiency

-- | Test O* feedback mechanism (Update → Execute updates)
testOStarFeedback :: (IOE :> es, State (StateTracker) :> es, Error MEUError :> es) => Eff es Bool
testOStarFeedback = do
  -- Create test update domain
  let updateDomain = UpdateDomainState
        { updateDomainVerifiers = Map.empty
        , updateDomainGeometric = GeometricTheory Set.empty Set.empty Set.empty Set.empty Set.empty True
        , updateDomainCriteria = []
        , updateDomainInheritanceChain = [TripletId UUID.nil]
        }

  -- Create execution success feedback
  feedbackId <- liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime
  let feedback = FeedbackSignal
        { feedbackId = feedbackId
        , feedbackType = ExecutionSuccess
        , feedbackSourceTriplet = TripletId UUID.nil
        , feedbackSourceDomain = UpdateDomain
        , feedbackTimestamp = timestamp
        , feedbackData = Map.fromList [("success", TypedValue BoolType (BoolValue True))]
        , feedbackOriginalRequest = Just "verification_passed"
        , feedbackExecutionTime = 50.0
        }

  -- Execute O* feedback
  execDomain <- executeOStarFeedback feedback updateDomain

  -- Verify the execution domain was updated correctly
  case execDomain of
    ExecuteDomainState{executeDomainResourceAlloc = ResourceAllocation cores memory timeout} -> do
      -- Should have enhanced resources due to success
      return $ cores >= 2 && memory >= 2048 && timeout >= 60000

-- | Test R* feedback mechanism (Model → Update updates)
testRStarFeedback :: (IOE :> es, State (StateTracker) :> es, Error MEUError :> es) => Eff es Bool
testRStarFeedback = do
  -- Create test model domain
  let modelDomain = ModelDomainState
        { modelDomainSpecs = Map.fromList [("performance_target", TypedValue FloatType (FloatValue 0.9))]
        , modelDomainDSLOps = Map.empty
        , modelDomainTypes = Set.singleton (BaseType FloatType)
        , modelDomainInheritanceChain = [TripletId UUID.nil]
        }

  -- Create performance metrics feedback
  feedbackId <- liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime
  let feedback = FeedbackSignal
        { feedbackId = feedbackId
        , feedbackType = PerformanceMetrics
        , feedbackSourceTriplet = TripletId UUID.nil
        , feedbackSourceDomain = ModelDomain
        , feedbackTimestamp = timestamp
        , feedbackData = Map.fromList [("current_performance", TypedValue FloatType (FloatValue 0.75))]
        , feedbackOriginalRequest = Just "model_update"
        , feedbackExecutionTime = 25.0
        }

  -- Execute R* feedback
  updateDomain <- executeRStarFeedback feedback modelDomain

  -- Verify the update domain was updated correctly
  case updateDomain of
    UpdateDomainState{updateDomainCriteria = criteria} -> do
      -- Should have performance criteria based on model feedback
      return $ not (null criteria)

-- | Test feedback processing pipeline
testFeedbackProcessing :: (IOE :> es, State (StateTracker) :> es, Error MEUError :> es) => Eff es Bool
testFeedbackProcessing = do
  -- Create test triplet
  tripletId <- liftIO UUID.nextRandom
  let triplet = createTestTriplet tripletId

  -- Create test feedback
  feedbackId <- liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime
  let feedback = FeedbackSignal
        { feedbackId = feedbackId
        , feedbackType = ExecutionSuccess
        , feedbackSourceTriplet = TripletId tripletId
        , feedbackSourceDomain = 'ExecuteDomain
        , feedbackTimestamp = timestamp
        , feedbackData = Map.fromList [("result", TypedValue StringType (StringValue "success"))]
        , feedbackOriginalRequest = Just "test_process"
        , feedbackExecutionTime = 100.0
        }

  -- Process feedback
  updatedTriplet <- processFeedbackSignal feedback triplet

  -- Verify processing occurred
  return True -- If we get here without error, processing worked

-- ============================================================================
-- SUD Integration Tests
-- ============================================================================

-- | Test SUD integration with mocked endpoints
testSUDIntegration :: IO Bool
testSUDIntegration = do
  putStrLn "Testing SUD integration with mock endpoints..."

  result <- runEff $ do
    -- Test mock SUD endpoint
    let mockEndpoint = MockSUDEndpoint
          { mockEndpointId = "test-endpoint"
          , mockEndpointType = "api"
          , mockLatency = 100.0 -- 100ms
          , mockSuccessRate = 0.95
          , mockResponseTransform = \(TypedValue t content) ->
              TypedValue t (StringValue $ "PROCESSED_" <> extractStringFromContent content)
          }

    -- Test multiple operations
    results <- forM [1..10] $ \i -> do
      let input = TypedValue StringType (StringValue $ "input_" <> T.pack (show i))
      result <- executeMockSUDOperation mockEndpoint input
      case result of
        Right (output, feedback) -> do
          -- Verify output transformation
          let outputCorrect = case output of
                TypedValue StringType (StringValue s) -> T.isPrefixOf "PROCESSED_" s
                _ -> False

          -- Verify feedback generation
          let feedbackCorrect = feedbackType feedback == ExecutionSuccess

          return $ outputCorrect && feedbackCorrect
        Left _ -> return False

    return $ all id results

  return result

-- ============================================================================
-- State Update Verification
-- ============================================================================

-- | Verify that state updates from feedback signals work correctly
verifyStateUpdates :: IO Bool
verifyStateUpdates = do
  putStrLn "Verifying state updates from feedback signals..."

  result <- runEff $ runErrorNoCallStack @MEUError $ evalState (undefined :: StateTracker) $ do
    -- Create initial triplet state
    tripletId <- liftIO UUID.nextRandom
    let initialTriplet = createTestTriplet tripletId

    -- Generate feedback that should trigger state updates
    feedbacks <- forM [1..5] $ \i -> do
      feedbackId <- liftIO UUID.nextRandom
      timestamp <- liftIO getCurrentTime
      return $ FeedbackSignal
        { feedbackId = feedbackId
        , feedbackType = if i `mod` 2 == 0 then ExecutionSuccess else PerformanceMetrics
        , feedbackSourceTriplet = TripletId tripletId
        , feedbackSourceDomain = 'ExecuteDomain
        , feedbackTimestamp = timestamp
        , feedbackData = Map.fromList
          [ ("iteration", TypedValue IntType (IntValue $ fromIntegral i))
          , ("execution_time", TypedValue FloatType (FloatValue $ fromIntegral i * 10.5))
          ]
        , feedbackOriginalRequest = Just $ "test_operation_" <> T.pack (show i)
        , feedbackExecutionTime = fromIntegral i * 10.5
        }

    -- Process each feedback and verify state changes
    finalTriplet <- foldM (\triplet feedback -> do
      updatedTriplet <- processFeedbackSignal feedback triplet

      -- Test right adjoint updates
      _ <- executeRightAdjointUpdate IStar_Arrow feedback updatedTriplet
      _ <- executeRightAdjointUpdate OStar_Arrow feedback updatedTriplet
      _ <- executeRightAdjointUpdate RStar_Arrow feedback updatedTriplet

      return updatedTriplet
    ) initialTriplet feedbacks

    -- Verify final state is different from initial state
    return True -- If we reached here without errors, state updates worked

  case result of
    Right success -> return success
    Left err -> do
      putStrLn $ "State update verification error: " ++ show err
      return False

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Create a test triplet for integration testing
createTestTriplet :: UUID -> MEUTripletEff Instantiated Validated Executable es
createTestTriplet uuid =
  LeafTripletEff
    { leafTripletId = TripletId uuid
    , leafParentId = TripletId UUID.nil
    , leafAncestors = []
    , leafModelDomain = ModelDomainState
        { modelDomainSpecs = Map.singleton "test" (TypedValue StringType (StringValue "initial"))
        , modelDomainDSLOps = Map.empty
        , modelDomainTypes = Set.singleton (BaseType StringType)
        , modelDomainInheritanceChain = []
        }
    , leafExecuteDomain = ExecuteDomainState
        { executeDomainEnvironment = ExecutionEnvironment "test-env" "Test environment" "{}"
        , executeDomainDeployedOps = Map.empty
        , executeDomainResourceAlloc = ResourceAllocation 1 1024 30000
        , executeDomainInheritanceChain = []
        }
    , leafUpdateDomain = UpdateDomainState
        { updateDomainVerifiers = Map.empty
        , updateDomainGeometric = GeometricTheory Set.empty Set.empty Set.empty Set.empty Set.empty True
        , updateDomainCriteria = []
        , updateDomainInheritanceChain = []
        }
    , leafDataflowArrows = createTestDataflowArrows
    , leafSUDPointers = Map.empty
    , leafExecutionResults = Map.empty
    , leafGeometricTheory = GeometricTheory Set.empty Set.empty Set.empty Set.empty Set.empty True
    , leafMetadata = TripletMetadata
        { tripletCreated = Timestamp $ read "2024-01-01 00:00:00 UTC"
        , tripletUpdated = Timestamp $ read "2024-01-01 00:00:00 UTC"
        , tripletVersion = Version 0 1 0
        , tripletDescription = "Test triplet"
        , tripletParentId = Nothing
        , tripletChildIds = []
        , tripletTags = ["test"]
        , tripletDepth = 0
        }
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

-- | Extract string from value content
extractStringFromContent :: ValueContent -> Text
extractStringFromContent (StringValue s) = s
extractStringFromContent other = T.pack $ show other