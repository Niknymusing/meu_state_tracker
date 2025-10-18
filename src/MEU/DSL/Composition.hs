{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}

module MEU.DSL.Composition
  ( -- * DSL Function Composition
    DSLFunction(..)
  , DSLComposition(..)
  , CompositionGraph(..)

    -- * Function Registry and Management
  , FunctionRegistry(..)
  , registerDSLFunction
  , lookupFunction
  , findComposableFunctions
  , composeFunctions
  , validateComposition

    -- * Cross-Domain DSL
  , CrossDomainDSL(..)
  , DomainInternalDSL(..)
  , buildCrossDomainDSL
  , executeCrossDomainFunction

    -- * Function Execution with WS Integration
  , FunctionExecution(..)
  , ExecutionContext(..)
  , executeDSLFunction
  , recordExecution
  , updateWSFromExecution

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
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import MEU.Core.Types hiding (DSLFunction(..), DataflowArrowCollection(..))
import MEU.WS.StateTracker hiding (executeDSLFunction)
import MEU.DSL.DataflowArrows

-- | DSL Function with complete signature and metadata
data DSLFunction = DSLFunction
  { dslFuncId :: Text
  , dslFuncName :: Text
  , dslFuncDomain :: DomainId
  , dslFuncInputTypes :: [MEUType]
  , dslFuncOutputType :: MEUType
  , dslFuncImplementation :: SUDPointer
  , dslFuncDescription :: Text
  , dslFuncTripletId :: TripletId
  , dslFuncValidated :: Bool
  , dslFuncMetadata :: Map Text Text
  } deriving (Eq, Show, Generic)

-- | Composition of DSL functions
data DSLComposition = DSLComposition
  { compId :: Text
  , compSourceFunctions :: [DSLFunction]
  , compComposedFunction :: DSLFunction
  , compCompositionType :: CompositionType
  , compValidationResults :: [ValidationResult]
  } deriving (Eq, Show, Generic)

-- | Types of function composition
data CompositionType
  = SequentialComposition   -- f . g
  | ParallelComposition     -- f ||| g (parallel execution)
  | ConditionalComposition  -- if-then-else composition
  | DataflowComposition     -- Via dataflow arrows
  deriving (Eq, Show, Generic)

-- | Validation result for compositions
data ValidationResult = ValidationResult
  { validationPassed :: Bool
  , validationMessage :: Text
  , validationTimestamp :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Graph representing function composition relationships
data CompositionGraph = CompositionGraph
  { cgNodes :: Map Text DSLFunction
  , cgEdges :: Map Text (Text, Text) -- (source, target)
  , cgCompositions :: Map Text DSLComposition
  , cgLastUpdated :: Timestamp
  } deriving (Generic)

-- | Registry for managing DSL functions
data FunctionRegistry = FunctionRegistry
  { frFunctions :: Map Text DSLFunction
  , frByDomain :: Map DomainId [Text]
  , frByInputType :: Map MEUType [Text]
  , frByOutputType :: Map MEUType [Text]
  , frCompositions :: Map Text DSLComposition
  , frCompositionGraph :: CompositionGraph
  } deriving (Generic)

-- | Cross-domain DSL combining dataflow arrows and compositions
data CrossDomainDSL = CrossDomainDSL
  { cddArrows :: DataflowArrowCollection
  , cddInternalDSLs :: Map DomainId DomainInternalDSL
  , cddCompositions :: [DSLComposition]
  , cddTripletId :: TripletId
  } deriving (Generic)

-- | Domain-internal DSL
data DomainInternalDSL = DomainInternalDSL
  { didPrimitives :: Map Text DSLFunction
  , didCompositions :: Map Text DSLComposition
  , didTypes :: Set MEUType
  , didDomain :: DomainId
  } deriving (Generic)

-- | Function execution context
data ExecutionContext = ExecutionContext
  { ecTripletId :: TripletId
  , ecDomain :: DomainId
  , ecInputValues :: [TypedValue]
  , ecExpectedOutputType :: MEUType
  , ecExecutionId :: Text
  , ecTimestamp :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Function execution record
data FunctionExecution = FunctionExecution
  { feExecutionId :: Text
  , feFunctionId :: Text
  , feContext :: ExecutionContext
  , feInputValues :: [TypedValue]
  , feOutputValue :: Maybe TypedValue
  , feError :: Maybe MEUError
  , feStartTime :: Timestamp
  , feEndTime :: Maybe Timestamp
  , feSuccess :: Bool
  } deriving (Generic)

-- | Register a new DSL function
registerDSLFunction :: FunctionRegistry -> DSLFunction -> FunctionRegistry
registerDSLFunction registry func =
  let funcId = dslFuncId func
      domain = dslFuncDomain func
      inputTypes = dslFuncInputTypes func
      outputType = dslFuncOutputType func
  in registry
    { frFunctions = Map.insert funcId func (frFunctions registry)
    , frByDomain = Map.insertWith (++) domain [funcId] (frByDomain registry)
    , frByInputType = foldr (\inType -> Map.insertWith (++) inType [funcId])
                            (frByInputType registry) inputTypes
    , frByOutputType = Map.insertWith (++) outputType [funcId] (frByOutputType registry)
    }

-- | Look up function by ID
lookupFunction :: FunctionRegistry -> Text -> Maybe DSLFunction
lookupFunction registry funcId = Map.lookup funcId (frFunctions registry)

-- | Find functions that can be composed with a given function
findComposableFunctions :: FunctionRegistry -> DSLFunction -> [DSLFunction]
findComposableFunctions registry func =
  let outputType = dslFuncOutputType func
      candidateIds = Map.findWithDefault [] outputType (frByInputType registry)
      candidates = mapMaybe (lookupFunction registry) candidateIds
  in filter (\candidate ->
      outputType `elem` dslFuncInputTypes candidate &&
      dslFuncId candidate /= dslFuncId func
    ) candidates
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y:acc; Nothing -> acc) []

-- | Compose two DSL functions
composeFunctions :: DSLFunction -> DSLFunction -> Either MEUError DSLComposition
composeFunctions func1 func2 =
  if dslFuncOutputType func1 `elem` dslFuncInputTypes func2
    then Right $ DSLComposition
      { compId = dslFuncId func1 <> "_compose_" <> dslFuncId func2
      , compSourceFunctions = [func1, func2]
      , compComposedFunction = DSLFunction
          { dslFuncId = dslFuncId func1 <> "_composed_" <> dslFuncId func2
          , dslFuncName = dslFuncName func1 <> " ∘ " <> dslFuncName func2
          , dslFuncDomain = dslFuncDomain func1 -- Use source domain
          , dslFuncInputTypes = dslFuncInputTypes func1
          , dslFuncOutputType = dslFuncOutputType func2
          , dslFuncImplementation = dslFuncImplementation func1 -- Placeholder
          , dslFuncDescription = "Composition of " <> dslFuncName func1 <> " and " <> dslFuncName func2
          , dslFuncTripletId = dslFuncTripletId func1
          , dslFuncValidated = False -- Needs validation
          , dslFuncMetadata = Map.union (dslFuncMetadata func1) (dslFuncMetadata func2)
          }
      , compCompositionType = SequentialComposition
      , compValidationResults = []
      }
    else Left $ CompositionError $ "Incompatible function types: " <>
      T.pack (show (dslFuncOutputType func1)) <> " and " <> T.pack (show (dslFuncInputTypes func2))

-- | Validate function composition
validateComposition :: DSLComposition -> Bool
validateComposition comp =
  let sourceFuncs = compSourceFunctions comp
  in case sourceFuncs of
    [f1, f2] -> dslFuncOutputType f1 `elem` dslFuncInputTypes f2
    _ -> False -- Only handle binary composition for now

-- | Build cross-domain DSL from arrows and internal DSLs
buildCrossDomainDSL :: DataflowArrowCollection ->
                       Map DomainId DomainInternalDSL ->
                       TripletId ->
                       CrossDomainDSL
buildCrossDomainDSL arrows internalDSLs tripletId =
  let -- Create compositions from dataflow arrows
      arrowCompositions = createArrowCompositions arrows
  in CrossDomainDSL
    { cddArrows = arrows
    , cddInternalDSLs = internalDSLs
    , cddCompositions = arrowCompositions
    , cddTripletId = tripletId
    }

-- | Create compositions from dataflow arrows (placeholder implementation)
createArrowCompositions :: DataflowArrowCollection -> [DSLComposition]
createArrowCompositions arrows =
  -- For now, return empty list - would implement actual arrow composition logic
  []

-- | Execute cross-domain function
executeCrossDomainFunction :: CrossDomainDSL -> Text -> [TypedValue] -> IO (Either MEUError TypedValue)
executeCrossDomainFunction crossDSL funcId inputs = do
  -- Look for function in internal DSLs first
  case findFunctionInDSL crossDSL funcId of
    Just func -> executeDSLFunctionDirect func inputs
    Nothing -> return $ Left $ FunctionNotFoundError funcId

-- | Find function in cross-domain DSL
findFunctionInDSL :: CrossDomainDSL -> Text -> Maybe DSLFunction
findFunctionInDSL crossDSL funcId =
  let internalDSLs = Map.elems (cddInternalDSLs crossDSL)
      allFunctions = concatMap (Map.elems . didPrimitives) internalDSLs
  in case filter (\f -> dslFuncId f == funcId) allFunctions of
    (func:_) -> Just func
    [] -> Nothing

-- | Execute DSL function directly
executeDSLFunctionDirect :: DSLFunction -> [TypedValue] -> IO (Either MEUError TypedValue)
executeDSLFunctionDirect func inputs = do
  -- Validate input types
  let inputTypes = map getValueType inputs
      expectedTypes = dslFuncInputTypes func
  if inputTypes == expectedTypes
    then do
      -- Execute SUD function
      result <- case inputs of
        [input] -> executeSUDFunction (dslFuncImplementation func) input
        _ -> return $ Left $ ExecutionError "Multi-input functions not yet supported"
      case result of
        Left err -> return $ Left err
        Right output ->
          if getValueType output == dslFuncOutputType func
            then return $ Right output
            else return $ Left $ TypeMismatchError
              (getValueType output) (dslFuncOutputType func) "Function output"
    else return $ Left $ TypeMismatchError
      (if null inputTypes then UnitType else head inputTypes)
      (if null expectedTypes then UnitType else head expectedTypes)
      "Function input"

-- | Execute DSL function with full context and WS integration
executeDSLFunction :: WSStateTracker -> DSLFunction -> ExecutionContext -> IO FunctionExecution
executeDSLFunction tracker func context = do
  startTime <- Timestamp <$> getCurrentTime
  let execId = ecExecutionId context
      inputs = ecInputValues context

  -- Execute the function
  result <- executeDSLFunctionDirect func inputs

  endTime <- Timestamp <$> getCurrentTime

  let execution = case result of
        Left err -> FunctionExecution
          { feExecutionId = execId
          , feFunctionId = dslFuncId func
          , feContext = context
          , feInputValues = inputs
          , feOutputValue = Nothing
          , feError = Just err
          , feStartTime = startTime
          , feEndTime = Just endTime
          , feSuccess = False
          }
        Right output -> FunctionExecution
          { feExecutionId = execId
          , feFunctionId = dslFuncId func
          , feContext = context
          , feInputValues = inputs
          , feOutputValue = Just output
          , feError = Nothing
          , feStartTime = startTime
          , feEndTime = Just endTime
          , feSuccess = True
          }

  -- Record execution in WS State Tracker
  recordExecution tracker execution

  return execution

-- | Record function execution in WS State Tracker
recordExecution :: WSStateTracker -> FunctionExecution -> IO ()
recordExecution tracker execution = do
  let execId = feExecutionId execution
      success = feSuccess execution

  updateTracker tracker $ \state -> state
    { stateExecutionQueue = stateExecutionQueue state ++
        [execId <> if success then "_SUCCESS" else "_FAILED"]
    , statePointerCount = statePointerCount state + 1
    }

-- | Update WS State Tracker from function execution results
updateWSFromExecution :: WSStateTracker -> FunctionExecution -> IO ()
updateWSFromExecution tracker execution = do
  case feOutputValue execution of
    Just outputValue -> do
      -- Register new value in WS if execution succeeded
      let req = RegisterValueRequest
            (ValueId undefined) -- Would need to generate proper UUID
            outputValue
            (ecTripletId $ feContext execution)
      _ <- processAPIRequest tracker req
      return ()
    Nothing -> return () -- Handle error case if needed

-- Helper: Empty function registry
emptyFunctionRegistry :: FunctionRegistry
emptyFunctionRegistry = FunctionRegistry
  { frFunctions = Map.empty
  , frByDomain = Map.empty
  , frByInputType = Map.empty
  , frByOutputType = Map.empty
  , frCompositions = Map.empty
  , frCompositionGraph = CompositionGraph Map.empty Map.empty Map.empty
      (Timestamp $ read "2024-01-01 00:00:00 UTC")
  }