{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MEU.WS.FeedbackMechanism
  ( -- * Feedback Processing
    processFeedbackSignal
  , updateTripletFromFeedback
  , executeRightAdjointUpdate

    -- * Right Adjoint Operations (I*, O*, R*)
  , executeIStarFeedback
  , executeOStarFeedback
  , executeRStarFeedback

    -- * Feedback Types
  , FeedbackSignal(..)
  , FeedbackType(..)
  , ExecutionResult(..)
  , StateUpdateResult(..)

    -- * Testing and Mocking
  , MockSUDEndpoint(..)
  , executeMockSUDOperation
  , generateFeedbackFromExecution
  ) where

import Control.Monad (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import GHC.Generics (Generic)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Effectful
import Effectful.State.Static.Local
import Effectful.Error.Static
import Effectful.Reader.Static

import MEU.Core.Types
import MEU.WS.StateTracker (TrackerState)
import MEU.Core.Types (BaseType(..), MEUType(..), TypedValue(..), ValueContent(..), DomainState(..), ModelDomainState(..), ExecuteDomainState(..), UpdateDomainState(..), AcceptanceCriteria(..), GeometricTheory(..), ResourceAllocation(..), ExecutionEnvironment(..), TripletId(..), DomainType(..), ArrowType(..), SUDPointer(..), MEUError(..), MEUComputation)

-- ============================================================================
-- Feedback Signal Types
-- ============================================================================

-- | Types of feedback signals from SUD execution
data FeedbackType
  = ExecutionSuccess
  | ExecutionFailure
  | PerformanceMetrics
  | ConfigurationChange
  | ResourceUtilization
  deriving (Eq, Show, Generic)

-- | Feedback signal from SUD execution
data FeedbackSignal = FeedbackSignal
  { feedbackId :: UUID
  , feedbackType :: FeedbackType
  , feedbackSourceTriplet :: TripletId
  , feedbackSourceDomain :: DomainType
  , feedbackTimestamp :: UTCTime
  , feedbackData :: Map Text TypedValue
  , feedbackOriginalRequest :: Maybe Text -- Original DSL operation
  , feedbackExecutionTime :: Double -- microseconds
  } deriving (Eq, Show, Generic)

-- | Result of processing feedback
data StateUpdateResult = StateUpdateResult
  { updateSuccess :: Bool
  , updatesDomain :: DomainType
  , updatesApplied :: [Text]
  , updateErrors :: [MEUError]
  , inheritanceUpdates :: Map TripletId StateUpdateResult
  } deriving (Eq, Show, Generic)

-- | Mock SUD endpoint for testing
data MockSUDEndpoint = MockSUDEndpoint
  { mockEndpointId :: Text
  , mockEndpointType :: Text
  , mockLatency :: Double -- milliseconds
  , mockSuccessRate :: Double -- 0.0 to 1.0
  , mockResponseTransform :: TypedValue -> TypedValue
  }

-- ============================================================================
-- Right Adjoint Feedback Mechanisms (I*, O*, R*)
-- ============================================================================

-- | Execute I* feedback: Update model configuration based on execution environment feedback
executeIStarFeedback ::
  (IOE :> es, State (TrackerState) :> es, Error MEUError :> es) =>
  FeedbackSignal ->
  DomainState 'ExecuteDomain capability es ->
  MEUComputation es (DomainState 'ModelDomain capability es)
executeIStarFeedback feedback execDomain = do
  case feedback of
    FeedbackSignal{feedbackType = PerformanceMetrics, feedbackData = metrics} -> do
      -- Update model based on execution performance
      let newSpecs = Map.fromList
            [ ("last_execution_time", "150.5")
            , ("performance_score", "0.85")
            , ("resource_efficiency", "0.92")
            ]

      return $ ModelDomainState
        { modelDomainSpecs = newSpecs
        , modelDomainDSLOps = Map.empty
        , modelDomainTypes = Set.singleton (BaseType FloatType)
        , modelDomainInheritanceChain = executeDomainInheritanceChain execDomain
        }

    FeedbackSignal{feedbackType = ConfigurationChange, feedbackData = configData} -> do
      -- Update model configuration based on environment changes
      let configSpecs = Map.mapKeys ("config_" <>) configData
      return $ ModelDomainState
        { modelDomainSpecs = Map.mapKeys ("config_" <>) $ Map.map (T.pack . show) configData
        , modelDomainDSLOps = Map.empty
        , modelDomainTypes = Set.singleton (BaseType StringType)
        , modelDomainInheritanceChain = executeDomainInheritanceChain execDomain
        }

    _ -> do
      -- Default: create minimal model state
      return $ ModelDomainState
        { modelDomainSpecs = Map.singleton "feedback_processed" "true"
        , modelDomainDSLOps = Map.empty
        , modelDomainTypes = Set.empty
        , modelDomainInheritanceChain = executeDomainInheritanceChain execDomain
        }

-- | Execute O* feedback: Update execution environment logging based on update domain feedback
executeOStarFeedback ::
  (IOE :> es, State (TrackerState) :> es, Error MEUError :> es) =>
  FeedbackSignal ->
  DomainState 'UpdateDomain capability es ->
  MEUComputation es (DomainState 'ExecuteDomain capability es)
executeOStarFeedback feedback updateDomain = do
  case feedback of
    FeedbackSignal{feedbackType = ExecutionSuccess, feedbackData = successData} -> do
      -- Update execution environment based on successful verification
      let newResourceAlloc = ResourceAllocation 2 2048 60000 -- Increase resources
      return $ ExecuteDomainState
        { executeDomainEnvironment = ExecutionEnvironment "enhanced-env" "Environment enhanced based on success" "{\"enhanced\": true}"
        , executeDomainDeployedOps = Map.empty
        , executeDomainResourceAlloc = newResourceAlloc
        , executeDomainInheritanceChain = updateDomainInheritanceChain updateDomain
        }

    FeedbackSignal{feedbackType = ExecutionFailure, feedbackData = failureData} -> do
      -- Reduce resources and add debugging
      let debugResourceAlloc = ResourceAllocation 1 512 30000 -- Reduce resources
      return $ ExecuteDomainState
        { executeDomainEnvironment = ExecutionEnvironment "debug-env" "Environment in debug mode" "{\"debug\": true}"
        , executeDomainDeployedOps = Map.empty
        , executeDomainResourceAlloc = debugResourceAlloc
        , executeDomainInheritanceChain = updateDomainInheritanceChain updateDomain
        }

    _ -> do
      -- Default execution environment
      return $ ExecuteDomainState
        { executeDomainEnvironment = ExecutionEnvironment "default-env" "Default environment" "{}"
        , executeDomainDeployedOps = Map.empty
        , executeDomainResourceAlloc = ResourceAllocation 1 1024 30000
        , executeDomainInheritanceChain = updateDomainInheritanceChain updateDomain
        }

-- | Execute R* feedback: Update verification criteria based on model changes
executeRStarFeedback ::
  (IOE :> es, State (TrackerState) :> es, Error MEUError :> es) =>
  FeedbackSignal ->
  DomainState 'ModelDomain capability es ->
  MEUComputation es (DomainState 'UpdateDomain capability es)
executeRStarFeedback feedback modelDomain = do
  case feedback of
    FeedbackSignal{feedbackType = PerformanceMetrics, feedbackData = metrics} -> do
      -- Create new verification criteria based on model performance
      let newCriteria = [AcceptanceCriteria "performance_criteria" "Performance must meet SLA" ["execution_time < 1000ms"] []]
      let newVerifiers = Map.fromList [("performance_check", return True)]

      return $ UpdateDomainState
        { updateDomainVerifiers = newVerifiers
        , updateDomainGeometric = GeometricTheory Set.empty Set.empty Set.empty Set.empty Set.empty True
        , updateDomainCriteria = newCriteria
        , updateDomainInheritanceChain = modelDomainInheritanceChain modelDomain
        }

    _ -> do
      -- Default update domain
      return $ UpdateDomainState
        { updateDomainVerifiers = Map.empty
        , updateDomainGeometric = GeometricTheory Set.empty Set.empty Set.empty Set.empty Set.empty True
        , updateDomainCriteria = []
        , updateDomainInheritanceChain = modelDomainInheritanceChain modelDomain
        }

-- ============================================================================
-- Feedback Processing Pipeline
-- ============================================================================

-- | Process a feedback signal and update the appropriate MEU triplet
processFeedbackSignal ::
  (IOE :> es, State (TrackerState) :> es, Error MEUError :> es) =>
  FeedbackSignal ->
  MEUTripletEff inst val exec es ->
  MEUComputation es (MEUTripletEff inst val exec es)
processFeedbackSignal feedback triplet = do
  case feedbackSourceDomain feedback of
    ModelDomain -> updateTripletFromFeedback feedback triplet
    ExecuteDomain -> updateTripletFromFeedback feedback triplet
    UpdateDomain -> updateTripletFromFeedback feedback triplet

-- | Update triplet state based on feedback signal
updateTripletFromFeedback ::
  (IOE :> es, State (TrackerState) :> es, Error MEUError :> es) =>
  FeedbackSignal ->
  MEUTripletEff inst val exec es ->
  MEUComputation es (MEUTripletEff inst val exec es)
updateTripletFromFeedback feedback triplet@(LeafTripletEff{leafModelDomain = modelDomain, leafExecuteDomain = execDomain, leafUpdateDomain = updateDomain}) = do
  case feedbackSourceDomain feedback of
    ExecuteDomain -> do
      -- Apply I* feedback: E -> M
      newModelDomain <- executeIStarFeedback feedback execDomain
      return $ triplet { leafModelDomain = newModelDomain }

    UpdateDomain -> do
      -- Apply O* feedback: U -> E
      newExecDomain <- executeOStarFeedback feedback updateDomain
      return $ triplet { leafExecuteDomain = newExecDomain }

    ModelDomain -> do
      -- Apply R* feedback: M -> U
      newUpdateDomain <- executeRStarFeedback feedback modelDomain
      return $ triplet { leafUpdateDomain = newUpdateDomain }

updateTripletFromFeedback _ triplet = return triplet -- For non-leaf triplets

-- | Execute right adjoint update operations
executeRightAdjointUpdate ::
  (IOE :> es, State (TrackerState) :> es, Error MEUError :> es) =>
  ArrowType ->
  FeedbackSignal ->
  MEUTripletEff inst val exec es ->
  MEUComputation es StateUpdateResult
executeRightAdjointUpdate arrowType feedback triplet = do
  _ <- updateTripletFromFeedback feedback triplet
  case arrowType of
    IStar_Arrow -> -- I*: E→M
      return $ StateUpdateResult True ModelDomain ["model_config_updated"] [] Map.empty
    OStar_Arrow -> -- O*: U→E
      return $ StateUpdateResult True ExecuteDomain ["execution_env_updated"] [] Map.empty
    RStar_Arrow -> -- R*: M→U
      return $ StateUpdateResult True UpdateDomain ["verification_criteria_updated"] [] Map.empty
    _ ->
      return $ StateUpdateResult False ModelDomain [] [UnknownError "Invalid arrow type for feedback"] Map.empty

-- ============================================================================
-- Mock SUD Operations for Testing
-- ============================================================================

-- | Execute a mock SUD operation with simulated latency and feedback
executeMockSUDOperation ::
  (IOE :> es) =>
  MockSUDEndpoint ->
  TypedValue ->
  MEUComputation es (Either MEUError (TypedValue, FeedbackSignal))
executeMockSUDOperation mockEndpoint input = do
  startTime <- liftIO getCurrentTime

  -- Simulate latency
  liftIO $ do
    let delayMicros = round (mockLatency mockEndpoint * 1000)
    -- Use a simple delay simulation (in real implementation would use proper timing)
    return ()

  endTime <- liftIO getCurrentTime

  -- Simulate success/failure based on success rate
  feedbackId <- liftIO UUID.nextRandom

  let executionTimeMicros = realToFrac $ diffTime endTime startTime

  -- Transform input based on mock endpoint
  let transformedOutput = mockResponseTransform mockEndpoint input

  -- Generate feedback signal
  let feedback = FeedbackSignal
        { feedbackId = feedbackId
        , feedbackType = ExecutionSuccess
        , feedbackSourceTriplet = TripletId UUID.nil
        , feedbackSourceDomain = ExecuteDomain
        , feedbackTimestamp = endTime
        , feedbackData = Map.fromList
          [ ("execution_time", TypedValue (BaseType FloatType) (FloatValue executionTimeMicros))
          , ("input_size", TypedValue (BaseType IntType) (IntValue 42))
          , ("output_size", TypedValue (BaseType IntType) (IntValue 84))
          ]
        , feedbackOriginalRequest = Just (mockEndpointId mockEndpoint)
        , feedbackExecutionTime = executionTimeMicros
        }

  return $ Right (transformedOutput, feedback)
  where
    diffTime t2 t1 = realToFrac $ diffUTCTime t2 t1

-- | Generate feedback signal from execution results
generateFeedbackFromExecution ::
  (IOE :> es) =>
  TripletId ->
  DomainType ->
  Either MEUError TypedValue ->
  Double ->
  MEUComputation es FeedbackSignal
generateFeedbackFromExecution tripletId domain result execTime = do
  feedbackId <- liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime

  let (fType, fData) = case result of
        Right value ->
          ( ExecutionSuccess
          , Map.fromList
            [ ("result", value)
            , ("execution_time", TypedValue (BaseType FloatType) (FloatValue execTime))
            , ("success", TypedValue (BaseType BoolType) (BoolValue True))
            ]
          )
        Left err ->
          ( ExecutionFailure
          , Map.fromList
            [ ("error", TypedValue (BaseType StringType) (StringValue $ T.pack $ show err))
            , ("execution_time", TypedValue (BaseType FloatType) (FloatValue execTime))
            , ("success", TypedValue (BaseType BoolType) (BoolValue False))
            ]
          )

  return $ FeedbackSignal
    { feedbackId = feedbackId
    , feedbackType = fType
    , feedbackSourceTriplet = tripletId
    , feedbackSourceDomain = domain
    , feedbackTimestamp = timestamp
    , feedbackData = fData
    , feedbackOriginalRequest = Nothing
    , feedbackExecutionTime = execTime
    }

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Extract typed value from feedback data map
extractValueFromData :: Text -> Map Text TypedValue -> TypedValue
extractValueFromData key dataMap =
  Map.findWithDefault (TypedValue UnitType NullValue) key dataMap