{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}

module MEU.API.SUDIntegration
  ( -- * SUD API Integration
    SUDAPIRequest(..)
  , SUDAPIResponse(..)
  , SUDAPIEndpoint(..)
  , SUDIntegrationConfig(..)

    -- * Function Pointer Execution
  , FunctionPointerExecution(..)
  , PointerExecutionResult(..)
  , executePointer
  , relayOutputSignals
  , recordExternalResults

    -- * WS State Tracker API Endpoints
  , WSAPIEndpoint(..)
  , setupAPIEndpoints
  , handleAPIRequest
  , triggerDSLExecution
  , updateWSFromExternal

    -- * Signal Relaying and Result Recording
  , OutputSignal(..)
  , ExternalResult(..)
  , SignalRelay(..)
  , processOutputSignals
  , integrateExternalResults

    -- * Pointer Management
  , PointerManager(..)
  , createPointerManager
  , registerPointer
  , executePointerByID
  , updatePointerStatus

  ) where

import Control.Concurrent.STM (STM, TVar, readTVar, readTVarIO, writeTVar, atomically, newTVar)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)

import MEU.Core.Types
import MEU.WS.StateTracker hiding (UpdatePointerRequest)
import MEU.DSL.DataflowArrows
import MEU.DSL.Composition

-- | SUD API Request types
data SUDAPIRequest
  = ExecuteFunctionRequest
      { sudReqFunctionId :: Text
      , sudReqInputs :: [TypedValue]
      , sudReqTripletId :: TripletId
      , sudReqExecutionId :: Text
      }
  | RegisterEndpointRequest
      { sudReqEndpointId :: Text
      , sudReqEndpoint :: SUDEndpoint
      , sudReqMetadata :: Map Text Text
      }
  | QueryPointerRequest
      { sudReqPointerId :: Text
      }
  | UpdatePointerRequest
      { sudReqPointerId :: Text
      , sudReqPointer :: SUDPointer
      }
  deriving (Eq, Show, Generic)

-- | SUD API Response types
data SUDAPIResponse
  = FunctionExecutedResponse
      { sudRespExecutionId :: Text
      , sudRespResult :: Either MEUError TypedValue
      , sudRespTimestamp :: Timestamp
      }
  | EndpointRegisteredResponse
      { sudRespEndpointId :: Text
      , sudRespStatus :: Text
      }
  | PointerQueryResponse
      { sudRespPointer :: Maybe SUDPointer
      }
  | PointerUpdatedResponse
      { sudRespPointerId :: Text
      , sudRespSuccess :: Bool
      }
  deriving (Eq, Show, Generic)

-- | SUD API Endpoint configuration
data SUDAPIEndpoint = SUDAPIEndpoint
  { saeEndpointId :: Text
  , saeEndpointType :: Text
  , saeBaseURL :: Text
  , saeAuthConfig :: Maybe Text
  , saeTimeoutMs :: Int
  , saeRetryCount :: Int
  , saeActive :: Bool
  } deriving (Eq, Show, Generic)

-- | SUD Integration configuration
data SUDIntegrationConfig = SUDIntegrationConfig
  { sicEndpoints :: Map Text SUDAPIEndpoint
  , sicDefaultTimeout :: Int
  , sicMaxRetries :: Int
  , sicLoggingEnabled :: Bool
  , sicAuthTokens :: Map Text Text
  } deriving (Generic)

-- | Function pointer execution context
data FunctionPointerExecution = FunctionPointerExecution
  { fpeExecutionId :: Text
  , fpePointerId :: Text
  , fpePointer :: SUDPointer
  , fpeInputs :: [TypedValue]
  , fpeStartTime :: Timestamp
  , fpeEndTime :: Maybe Timestamp
  , fpeStatus :: ExecutionStatus
  } deriving (Generic)

-- | Execution status
data ExecutionStatus
  = ExecutionPending
  | ExecutionRunning
  | ExecutionCompleted
  | ExecutionFailed
  | ExecutionTimeout
  deriving (Eq, Show, Generic)

-- | Pointer execution result
data PointerExecutionResult = PointerExecutionResult
  { perExecutionId :: Text
  , perSuccess :: Bool
  , perResult :: Maybe TypedValue
  , perError :: Maybe MEUError
  , perOutputSignals :: [OutputSignal]
  , perTimestamp :: Timestamp
  } deriving (Generic)

-- | WS State Tracker API Endpoint
data WSAPIEndpoint = WSAPIEndpoint
  { waeEndpointId :: Text
  , waeEndpointPath :: Text
  , waeMethod :: Text -- GET, POST, etc.
  , waeHandler :: SUDAPIRequest -> IO SUDAPIResponse
  , waeDescription :: Text
  , waeActive :: Bool
  }

-- | Output signal from WS State Tracker to SUD
data OutputSignal = OutputSignal
  { osSignalId :: Text
  , osSourceTripletId :: TripletId
  , osTargetPointerId :: Text
  , osPayload :: TypedValue
  , osSignalType :: SignalType
  , osTimestamp :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Types of output signals
data SignalType
  = ExecutionSignal    -- Trigger function execution
  | ConfigurationSignal -- Update configuration
  | DataSignal         -- Data transfer
  | TerminationSignal  -- Stop execution
  deriving (Eq, Show, Generic)

-- | External result from SUD execution
data ExternalResult = ExternalResult
  { erResultId :: Text
  , erSourcePointerId :: Text
  , erExecutionId :: Text
  , erResultValue :: TypedValue
  , erMetadata :: Map Text Text
  , erTimestamp :: Timestamp
  , erSuccess :: Bool
  } deriving (Eq, Show, Generic)

-- | Signal relay mechanism
data SignalRelay = SignalRelay
  { srRelayId :: Text
  , srSourceSignals :: [OutputSignal]
  , srTargetPointers :: [SUDPointer]
  , srRelayStatus :: RelayStatus
  , srResults :: [ExternalResult]
  } deriving (Generic)

-- | Relay status
data RelayStatus
  = RelayPending
  | RelayActive
  | RelayCompleted
  | RelayFailed
  deriving (Eq, Show, Generic)

-- | Pointer manager for SUD function management
data PointerManager = PointerManager
  { pmPointers :: TVar (Map Text SUDPointer)
  , pmExecutions :: TVar (Map Text FunctionPointerExecution)
  , pmConfig :: SUDIntegrationConfig
  , pmLastUpdated :: TVar Timestamp
  } deriving (Generic)

-- | Execute function pointer
executePointer :: PointerManager -> SUDPointer -> [TypedValue] -> IO PointerExecutionResult
executePointer manager pointer inputs = do
  execId <- ("exec_" <>) . T.pack . show <$> getCurrentTime
  startTime <- Timestamp <$> getCurrentTime

  let execution = FunctionPointerExecution
        { fpeExecutionId = execId
        , fpePointerId = sudPointerId pointer
        , fpePointer = pointer
        , fpeInputs = inputs
        , fpeStartTime = startTime
        , fpeEndTime = Nothing
        , fpeStatus = ExecutionRunning
        }

  -- Record execution start
  atomically $ do
    executions <- readTVar (pmExecutions manager)
    writeTVar (pmExecutions manager) (Map.insert execId execution executions)

  -- Execute the function
  result <- case inputs of
    [input] -> executeSUDFunction pointer input
    _ -> return $ Left $ ExecutionError "Multi-input execution not supported"

  endTime <- Timestamp <$> getCurrentTime

  -- Update execution status
  let finalStatus = case result of
        Left _ -> ExecutionFailed
        Right _ -> ExecutionCompleted

  atomically $ do
    executions <- readTVar (pmExecutions manager)
    let updatedExecution = execution
          { fpeEndTime = Just endTime
          , fpeStatus = finalStatus
          }
    writeTVar (pmExecutions manager) (Map.insert execId updatedExecution executions)

  -- Create output signals if successful
  outputSignals <- case result of
    Right value -> return [OutputSignal
      { osSignalId = execId <> "_output"
      , osSourceTripletId = TripletId undefined -- Should be provided
      , osTargetPointerId = sudPointerId pointer
      , osPayload = value
      , osSignalType = DataSignal
      , osTimestamp = endTime
      }]
    Left _ -> return []

  return PointerExecutionResult
    { perExecutionId = execId
    , perSuccess = case result of { Right _ -> True; Left _ -> False }
    , perResult = case result of { Right v -> Just v; Left _ -> Nothing }
    , perError = case result of { Left e -> Just e; Right _ -> Nothing }
    , perOutputSignals = outputSignals
    , perTimestamp = endTime
    }

-- | Relay output signals to external functions
relayOutputSignals :: WSStateTracker -> PointerManager -> [OutputSignal] -> IO [ExternalResult]
relayOutputSignals tracker manager signals = do
  results <- mapM (processSignal manager) signals
  -- Update WS State Tracker with results
  mapM_ (updateWSFromExternalResult tracker) results
  return results

-- | Process individual output signal
processSignal :: PointerManager -> OutputSignal -> IO ExternalResult
processSignal manager signal = do
  pointers <- readTVarIO (pmPointers manager)
  case Map.lookup (osTargetPointerId signal) pointers of
    Just pointer -> do
      -- Execute the target function
      execResult <- executePointer manager pointer [osPayload signal]
      let success = perSuccess execResult
          value = maybe (TypedValue UnitType NullValue) id (perResult execResult)
      return ExternalResult
        { erResultId = osSignalId signal <> "_result"
        , erSourcePointerId = osTargetPointerId signal
        , erExecutionId = perExecutionId execResult
        , erResultValue = value
        , erMetadata = Map.empty
        , erTimestamp = perTimestamp execResult
        , erSuccess = success
        }
    Nothing -> do
      timestamp <- Timestamp <$> getCurrentTime
      return ExternalResult
        { erResultId = osSignalId signal <> "_error"
        , erSourcePointerId = osTargetPointerId signal
        , erExecutionId = "none"
        , erResultValue = TypedValue (BaseType StringType) (StringValue "Pointer not found")
        , erMetadata = Map.singleton "error" "pointer_not_found"
        , erTimestamp = timestamp
        , erSuccess = False
        }

-- | Record external execution results
recordExternalResults :: WSStateTracker -> [ExternalResult] -> IO ()
recordExternalResults tracker results = do
  mapM_ (updateWSFromExternalResult tracker) results

-- | Update WS State Tracker from external result
updateWSFromExternalResult :: WSStateTracker -> ExternalResult -> IO ()
updateWSFromExternalResult tracker result = do
  if erSuccess result
    then do
      -- Register successful result as new value
      valueId <- ValueId <$> nextRandom
      tripletId <- TripletId <$> nextRandom
      let req = RegisterValueRequest
            valueId
            (erResultValue result)
            tripletId
      _ <- processAPIRequest tracker req
      return ()
    else do
      -- Update error state
      updateTracker tracker $ \state -> state
        { stateExecutionQueue = stateExecutionQueue state ++ ["ERROR_" <> erResultId result] }

-- | Setup API endpoints for WS State Tracker
setupAPIEndpoints :: WSStateTracker -> PointerManager -> [WSAPIEndpoint]
setupAPIEndpoints tracker manager =
  [ WSAPIEndpoint
      { waeEndpointId = "execute_function"
      , waeEndpointPath = "/api/execute"
      , waeMethod = "POST"
      , waeHandler = handleExecuteFunction tracker manager
      , waeDescription = "Execute DSL function through WS State Tracker"
      , waeActive = True
      }
  , WSAPIEndpoint
      { waeEndpointId = "register_pointer"
      , waeEndpointPath = "/api/register"
      , waeMethod = "POST"
      , waeHandler = handleRegisterPointer manager
      , waeDescription = "Register new SUD function pointer"
      , waeActive = True
      }
  , WSAPIEndpoint
      { waeEndpointId = "query_status"
      , waeEndpointPath = "/api/status"
      , waeMethod = "GET"
      , waeHandler = handleQueryStatus tracker
      , waeDescription = "Query WS State Tracker status"
      , waeActive = True
      }
  ]

-- | Handle function execution API request
handleExecuteFunction :: WSStateTracker -> PointerManager -> SUDAPIRequest -> IO SUDAPIResponse
handleExecuteFunction tracker manager req = case req of
  ExecuteFunctionRequest funcId inputs tripletId execId -> do
    -- Look up function in WS State Tracker
    -- This would integrate with the DSL function registry
    timestamp <- Timestamp <$> getCurrentTime

    -- For now, simulate execution
    let simulatedResult = TypedValue (BaseType StringType) (StringValue $ "Executed " <> funcId)

    return FunctionExecutedResponse
      { sudRespExecutionId = execId
      , sudRespResult = Right simulatedResult
      , sudRespTimestamp = timestamp
      }
  _ -> do
    timestamp <- Timestamp <$> getCurrentTime
    return FunctionExecutedResponse
      { sudRespExecutionId = "error"
      , sudRespResult = Left $ ExecutionError "Invalid request type"
      , sudRespTimestamp = timestamp
      }

-- | Handle pointer registration API request
handleRegisterPointer :: PointerManager -> SUDAPIRequest -> IO SUDAPIResponse
handleRegisterPointer manager req = case req of
  RegisterEndpointRequest endpointId endpoint metadata -> do
    -- Create pointer from endpoint
    let pointer = SUDPointer
          { sudPointerId = endpointId
          , sudEndpoint = endpoint
          , sudFunctionSignature = "auto_generated"
          , sudMetadata = metadata
          , sudActive = True
          }

    -- Register pointer
    atomically $ do
      pointers <- readTVar (pmPointers manager)
      writeTVar (pmPointers manager) (Map.insert endpointId pointer pointers)

    return EndpointRegisteredResponse
      { sudRespEndpointId = endpointId
      , sudRespStatus = "registered"
      }
  _ -> return EndpointRegisteredResponse
      { sudRespEndpointId = "error"
      , sudRespStatus = "invalid_request"
      }

-- | Handle status query API request
handleQueryStatus :: WSStateTracker -> SUDAPIRequest -> IO SUDAPIResponse
handleQueryStatus tracker req = do
  -- Query current WS State Tracker status
  timestamp <- Timestamp <$> getCurrentTime
  state <- atomically $ getTrackerState tracker

  let statusValue = TypedValue (BaseType StringType) (StringValue $
        "Active: " <> (if stateActive state then "true" else "false") <>
        ", Pointers: " <> (T.pack $ show $ statePointerCount state))

  return FunctionExecutedResponse
    { sudRespExecutionId = "status_query"
    , sudRespResult = Right statusValue
    , sudRespTimestamp = timestamp
    }

-- | Handle general API request routing
handleAPIRequest :: WSStateTracker -> PointerManager -> SUDAPIRequest -> IO SUDAPIResponse
handleAPIRequest tracker manager req = case req of
  ExecuteFunctionRequest{} -> handleExecuteFunction tracker manager req
  RegisterEndpointRequest{} -> handleRegisterPointer manager req
  QueryPointerRequest{} -> handleQueryStatus tracker req
  UpdatePointerRequest{} -> handleRegisterPointer manager req

-- | Trigger DSL execution through WS State Tracker
triggerDSLExecution :: WSStateTracker -> PointerManager -> Text -> [TypedValue] -> IO (Either MEUError TypedValue)
triggerDSLExecution tracker manager funcId inputs = do
  -- Create execution request
  let execId = funcId <> "_" <> (T.pack $ show (length inputs))
      req = ExecuteFunctionRequest funcId inputs (TripletId undefined) execId

  -- Execute through API
  response <- handleAPIRequest tracker manager req

  case response of
    FunctionExecutedResponse _ result _ -> return result
    _ -> return $ Left $ ExecutionError "Unexpected response type"

-- | Update WS State from external system
updateWSFromExternal :: WSStateTracker -> ExternalResult -> IO ()
updateWSFromExternal = updateWSFromExternalResult

-- | Process output signals
processOutputSignals :: PointerManager -> [OutputSignal] -> IO SignalRelay
processOutputSignals manager signals = do
  relayId <- ("relay_" <>) . show <$> getCurrentTime

  -- Process each signal
  results <- mapM (processSignal manager) signals

  return SignalRelay
    { srRelayId = T.pack relayId
    , srSourceSignals = signals
    , srTargetPointers = [] -- Would be populated from actual processing
    , srRelayStatus = RelayCompleted
    , srResults = results
    }

-- | Integrate external results into WS State Tracker
integrateExternalResults :: WSStateTracker -> [ExternalResult] -> IO ()
integrateExternalResults tracker results = do
  mapM_ (updateWSFromExternalResult tracker) results

-- | Create pointer manager
createPointerManager :: SUDIntegrationConfig -> IO PointerManager
createPointerManager config = do
  pointersVar <- newTVarIO Map.empty
  executionsVar <- newTVarIO Map.empty
  lastUpdatedVar <- newTVarIO =<< (Timestamp <$> getCurrentTime)

  return PointerManager
    { pmPointers = pointersVar
    , pmExecutions = executionsVar
    , pmConfig = config
    , pmLastUpdated = lastUpdatedVar
    }

-- | Register pointer in manager
registerPointer :: PointerManager -> SUDPointer -> IO ()
registerPointer manager pointer = do
  timestamp <- Timestamp <$> getCurrentTime
  atomically $ do
    pointers <- readTVar (pmPointers manager)
    writeTVar (pmPointers manager)
      (Map.insert (sudPointerId pointer) pointer pointers)
    writeTVar (pmLastUpdated manager) timestamp

-- | Execute pointer by ID
executePointerByID :: PointerManager -> Text -> [TypedValue] -> IO (Maybe PointerExecutionResult)
executePointerByID manager pointerId inputs = do
  pointers <- readTVarIO (pmPointers manager)
  case Map.lookup pointerId pointers of
    Just pointer -> Just <$> executePointer manager pointer inputs
    Nothing -> return Nothing

-- | Update pointer status
updatePointerStatus :: PointerManager -> Text -> Bool -> IO Bool
updatePointerStatus manager pointerId active = do
  atomically $ do
    pointers <- readTVar (pmPointers manager)
    case Map.lookup pointerId pointers of
      Just pointer -> do
        let updatedPointer = pointer { sudActive = active }
        writeTVar (pmPointers manager)
          (Map.insert pointerId updatedPointer pointers)
        return True
      Nothing -> return False

-- Helper: Create TVar in IO
newTVarIO :: a -> IO (TVar a)
newTVarIO = atomically . newTVar