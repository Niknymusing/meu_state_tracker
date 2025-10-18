{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}

module MEU.DSL.DataflowArrows
  ( -- * Cross-Domain Dataflow Arrows
    DataflowArrow(..)
  , ArrowType(..)
  , DataflowArrowCollection(..)

    -- * Model-Environment Arrows
  , ModelToEnvArrow(..)
  , EnvToModelArrow(..)
  , executeModelToEnv
  , executeEnvToModel

    -- * Environment-Update Arrows
  , EnvToUpdateArrow(..)
  , UpdateToEnvArrow(..)
  , executeEnvToUpdate
  , executeUpdateToEnv

    -- * Update-Model Arrows
  , UpdateToModelArrow(..)
  , ModelToUpdateArrow(..)
  , executeUpdateToModel
  , executeModelToUpdate

    -- * Arrow Composition
  , composeArrows
  , validateArrowComposition
  , createIdentityArrow
  , findComposableArrows

    -- * SUD Integration
  , SUDPointer(..)
  , SUDEndpoint(..)
  , executeSUDFunction
  , recordSUDResult
  , updateWSStateFromSUD

  ) where

import Control.Concurrent.STM (STM, TVar, readTVar, writeTVar, atomically)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)

import MEU.Core.Types hiding (DataflowArrow(..), ArrowType(..), arrowType, arrowSourceDomain, arrowTargetDomain, arrowFunction, DataflowArrowCollection(..))
import MEU.WS.StateTracker

-- | Types of dataflow arrows as per MEU specification
data ArrowType
  = IArrow    -- M -> E: Deploy models to execution environment
  | IStarArrow -- E -> M: Adapt models based on execution feedback
  | OArrow    -- E -> U: Extract feedback for evaluation
  | OStarArrow -- U -> E: Configure logging mechanisms
  | RArrow    -- U -> M: Update models based on evaluation
  | RStarArrow -- M -> U: Deploy evaluation mechanisms
  deriving (Eq, Show, Ord, Generic)

-- | Generic dataflow arrow with type signature and execution context
data DataflowArrow = DataflowArrow
  { arrowId :: Text
  , arrowType :: ArrowType
  , arrowSourceDomain :: DomainId
  , arrowTargetDomain :: DomainId
  , arrowInputType :: MEUType
  , arrowOutputType :: MEUType
  , arrowTripletId :: TripletId
  , arrowFunction :: SUDPointer -- Pointer to actual implementation
  , arrowMetadata :: Map Text Text
  } deriving (Eq, Show, Generic)

-- | Collection of all dataflow arrows for a MEU triplet
data DataflowArrowCollection = DataflowArrowCollection
  { dacModelToEnv :: Map Text ModelToEnvArrow      -- I arrows
  , dacEnvToModel :: Map Text EnvToModelArrow      -- I* arrows
  , dacEnvToUpdate :: Map Text EnvToUpdateArrow    -- O arrows
  , dacUpdateToEnv :: Map Text UpdateToEnvArrow    -- O* arrows
  , dacUpdateToModel :: Map Text UpdateToModelArrow -- R arrows
  , dacModelToUpdate :: Map Text ModelToUpdateArrow -- R* arrows
  , dacTripletId :: TripletId
  , dacLastUpdated :: Timestamp
  } deriving (Generic)

-- | Model to Environment arrows (I: M -> E)
data ModelToEnvArrow = ModelToEnvArrow
  { mteArrowId :: Text
  , mteInputType :: MEUType
  , mteOutputType :: MEUType
  , mteDeploymentConfig :: Map Text Text
  , mteExecutionEnvironment :: Text
  , mteSUDPointer :: SUDPointer
  , mteValidated :: Bool
  } deriving (Eq, Show, Generic)

-- | Environment to Model arrows (I*: E -> M)
data EnvToModelArrow = EnvToModelArrow
  { etmArrowId :: Text
  , etmInputType :: MEUType
  , etmOutputType :: MEUType
  , etmFeedbackProcessor :: SUDPointer
  , etmAdaptationRules :: [Text]
  , etmValidated :: Bool
  } deriving (Eq, Show, Generic)

-- | Environment to Update arrows (O: E -> U)
data EnvToUpdateArrow = EnvToUpdateArrow
  { etuArrowId :: Text
  , etuInputType :: MEUType
  , etuOutputType :: MEUType
  , etuExtractorFunction :: SUDPointer
  , etuLoggingConfig :: Map Text Text
  , etuValidated :: Bool
  } deriving (Eq, Show, Generic)

-- | Update to Environment arrows (O*: U -> E)
data UpdateToEnvArrow = UpdateToEnvArrow
  { uteArrowId :: Text
  , uteInputType :: MEUType
  , uteOutputType :: MEUType
  , uteConfigFunction :: SUDPointer
  , uteLoggingUpdates :: [Text]
  , uteValidated :: Bool
  } deriving (Eq, Show, Generic)

-- | Update to Model arrows (R: U -> M)
data UpdateToModelArrow = UpdateToModelArrow
  { utmArrowId :: Text
  , utmInputType :: MEUType
  , utmOutputType :: MEUType
  , utmUpdateFunction :: SUDPointer
  , utmVerificationRules :: [Text]
  , utmValidated :: Bool
  } deriving (Eq, Show, Generic)

-- | Model to Update arrows (R*: M -> U)
data ModelToUpdateArrow = ModelToUpdateArrow
  { mtuArrowId :: Text
  , mtuInputType :: MEUType
  , mtuOutputType :: MEUType
  , mtuVerifierDeployer :: SUDPointer
  , mtuAcceptanceCriteria :: [Text]
  , mtuValidated :: Bool
  } deriving (Eq, Show, Generic)

-- | SUD (System Under Development) Pointer
data SUDPointer = SUDPointer
  { sudPointerId :: Text
  , sudEndpoint :: SUDEndpoint
  , sudFunctionSignature :: Text
  , sudMetadata :: Map Text Text
  , sudActive :: Bool
  } deriving (Eq, Show, Generic)

-- | SUD Endpoint for external function execution
data SUDEndpoint = SUDEndpoint
  { sudEndpointType :: Text -- "api", "function", "process", etc.
  , sudEndpointAddress :: Text
  , sudEndpointAuth :: Maybe Text
  , sudEndpointTimeout :: Int -- milliseconds
  } deriving (Eq, Show, Generic)

-- | Execute Model to Environment arrow (I: M -> E)
executeModelToEnv :: ModelToEnvArrow -> TypedValue -> IO (Either MEUError TypedValue)
executeModelToEnv arrow input = do
  -- Validate input type
  if getValueType input /= mteInputType arrow
    then return $ Left $ TypeMismatchError
      (getValueType input) (mteInputType arrow) "ModelToEnv execution"
    else do
      -- Execute SUD function pointer
      result <- executeSUDFunction (mteSUDPointer arrow) input
      case result of
        Left err -> return $ Left err
        Right output -> do
          -- Validate output type
          if getValueType output == mteOutputType arrow
            then return $ Right output
            else return $ Left $ TypeMismatchError
              (getValueType output) (mteOutputType arrow) "ModelToEnv output"

-- | Execute Environment to Model arrow (I*: E -> M)
executeEnvToModel :: EnvToModelArrow -> TypedValue -> IO (Either MEUError TypedValue)
executeEnvToModel arrow input = do
  if getValueType input /= etmInputType arrow
    then return $ Left $ TypeMismatchError
      (getValueType input) (etmInputType arrow) "EnvToModel execution"
    else do
      result <- executeSUDFunction (etmFeedbackProcessor arrow) input
      case result of
        Left err -> return $ Left err
        Right output ->
          if getValueType output == etmOutputType arrow
            then return $ Right output
            else return $ Left $ TypeMismatchError
              (getValueType output) (etmOutputType arrow) "EnvToModel output"

-- | Execute Environment to Update arrow (O: E -> U)
executeEnvToUpdate :: EnvToUpdateArrow -> TypedValue -> IO (Either MEUError TypedValue)
executeEnvToUpdate arrow input = do
  if getValueType input /= etuInputType arrow
    then return $ Left $ TypeMismatchError
      (getValueType input) (etuInputType arrow) "EnvToUpdate execution"
    else do
      result <- executeSUDFunction (etuExtractorFunction arrow) input
      case result of
        Left err -> return $ Left err
        Right output ->
          if getValueType output == etuOutputType arrow
            then return $ Right output
            else return $ Left $ TypeMismatchError
              (getValueType output) (etuOutputType arrow) "EnvToUpdate output"

-- | Execute Update to Environment arrow (O*: U -> E)
executeUpdateToEnv :: UpdateToEnvArrow -> TypedValue -> IO (Either MEUError TypedValue)
executeUpdateToEnv arrow input = do
  if getValueType input /= uteInputType arrow
    then return $ Left $ TypeMismatchError
      (getValueType input) (uteInputType arrow) "UpdateToEnv execution"
    else do
      result <- executeSUDFunction (uteConfigFunction arrow) input
      case result of
        Left err -> return $ Left err
        Right output ->
          if getValueType output == uteOutputType arrow
            then return $ Right output
            else return $ Left $ TypeMismatchError
              (getValueType output) (uteOutputType arrow) "UpdateToEnv output"

-- | Execute Update to Model arrow (R: U -> M)
executeUpdateToModel :: UpdateToModelArrow -> TypedValue -> IO (Either MEUError TypedValue)
executeUpdateToModel arrow input = do
  if getValueType input /= utmInputType arrow
    then return $ Left $ TypeMismatchError
      (getValueType input) (utmInputType arrow) "UpdateToModel execution"
    else do
      result <- executeSUDFunction (utmUpdateFunction arrow) input
      case result of
        Left err -> return $ Left err
        Right output ->
          if getValueType output == utmOutputType arrow
            then return $ Right output
            else return $ Left $ TypeMismatchError
              (getValueType output) (utmOutputType arrow) "UpdateToModel output"

-- | Execute Model to Update arrow (R*: M -> U)
executeModelToUpdate :: ModelToUpdateArrow -> TypedValue -> IO (Either MEUError TypedValue)
executeModelToUpdate arrow input = do
  if getValueType input /= mtuInputType arrow
    then return $ Left $ TypeMismatchError
      (getValueType input) (mtuInputType arrow) "ModelToUpdate execution"
    else do
      result <- executeSUDFunction (mtuVerifierDeployer arrow) input
      case result of
        Left err -> return $ Left err
        Right output ->
          if getValueType output == mtuOutputType arrow
            then return $ Right output
            else return $ Left $ TypeMismatchError
              (getValueType output) (mtuOutputType arrow) "ModelToUpdate output"

-- | Execute SUD function pointer and record results
executeSUDFunction :: SUDPointer -> TypedValue -> IO (Either MEUError TypedValue)
executeSUDFunction pointer@(SUDPointer{sudActive = active, sudEndpoint = endpoint, sudPointerId = pointerId}) input = do
  -- This is a placeholder - in real implementation would call external API/function
  -- For now, return a simple transformation based on the pointer type
  if active
    then do
      -- Simulate external function execution
      let SUDEndpoint{sudEndpointType = endpointType} = endpoint
      let result = case endpointType of
            "api" -> TypedValue (getValueType input) (StringValue $ "API_RESULT_" <> extractStringValue input)
            "function" -> TypedValue (getValueType input) (StringValue $ "FUNC_RESULT_" <> extractStringValue input)
            "process" -> TypedValue (getValueType input) (StringValue $ "PROC_RESULT_" <> extractStringValue input)
            _ -> TypedValue UnitType NullValue
      return $ Right result
    else return $ Left $ SUDExecutionError $ "SUD pointer inactive: " <> pointerId

-- | Helper to extract string value (placeholder)
extractStringValue :: TypedValue -> Text
extractStringValue (TypedValue _ (StringValue s)) = s
extractStringValue (TypedValue _ _) = "unknown"

-- | Record SUD execution result in WS State Tracker
recordSUDResult :: WSStateTracker -> Text -> TypedValue -> TypedValue -> IO ()
recordSUDResult tracker arrowId input output = do
  -- Update tracker state with execution record
  updateTracker tracker $ \state -> state
    { stateExecutionQueue = stateExecutionQueue state ++ [arrowId <> "_executed"]
    , statePointerCount = statePointerCount state + 1
    }

-- | Update WS State Tracker based on SUD execution results
updateWSStateFromSUD :: WSStateTracker -> Text -> Either MEUError TypedValue -> IO ()
updateWSStateFromSUD tracker arrowId result = do
  case result of
    Left err -> do
      -- Log error and update state
      updateTracker tracker $ \state -> state
        { stateExecutionQueue = stateExecutionQueue state ++ ["ERROR_" <> arrowId] }
    Right value -> do
      -- Record successful execution
      updateTracker tracker $ \state -> state
        { stateExecutionQueue = stateExecutionQueue state ++ ["SUCCESS_" <> arrowId] }

-- | Compose two compatible arrows
composeArrows :: DataflowArrow -> DataflowArrow -> Either MEUError DataflowArrow
composeArrows arrow1 arrow2 =
  if arrowOutputType arrow1 == arrowInputType arrow2 &&
     arrowTargetDomain arrow1 == arrowSourceDomain arrow2
    then Right $ DataflowArrow
      { arrowId = arrowId arrow1 <> "_compose_" <> arrowId arrow2
      , arrowType = arrowType arrow1 -- Use first arrow's type as primary
      , arrowSourceDomain = arrowSourceDomain arrow1
      , arrowTargetDomain = arrowTargetDomain arrow2
      , arrowInputType = arrowInputType arrow1
      , arrowOutputType = arrowOutputType arrow2
      , arrowTripletId = arrowTripletId arrow1
      , arrowFunction = arrowFunction arrow1 -- Compose functions would be more complex
      , arrowMetadata = Map.union (arrowMetadata arrow1) (arrowMetadata arrow2)
      }
    else Left $ CompositionError $ "Incompatible arrows: " <> arrowId arrow1 <> " -> " <> arrowId arrow2

-- | Validate arrow composition compatibility
validateArrowComposition :: DataflowArrow -> DataflowArrow -> Bool
validateArrowComposition arrow1 arrow2 =
  arrowOutputType arrow1 == arrowInputType arrow2 &&
  arrowTargetDomain arrow1 == arrowSourceDomain arrow2

-- | Create identity arrow for a domain
createIdentityArrow :: DomainId -> MEUType -> TripletId -> DataflowArrow
createIdentityArrow domain meuType tripletId = DataflowArrow
  { arrowId = "identity_" <> (T.pack $ show domain)
  , arrowType = IArrow -- Default, should be parameterized
  , arrowSourceDomain = domain
  , arrowTargetDomain = domain
  , arrowInputType = meuType
  , arrowOutputType = meuType
  , arrowTripletId = tripletId
  , arrowFunction = SUDPointer "identity"
      (SUDEndpoint "function" "identity" Nothing 1000)
      "identity" Map.empty True
  , arrowMetadata = Map.singleton "type" "identity"
  }

-- | Find all composable arrows in a collection
findComposableArrows :: [DataflowArrow] -> [(DataflowArrow, DataflowArrow)]
findComposableArrows arrows =
  [(a1, a2) | a1 <- arrows, a2 <- arrows, a1 /= a2, validateArrowComposition a1 a2]