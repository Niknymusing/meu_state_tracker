{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module MEU.Internal.Utils
  ( -- * Utility Functions
    generateUUID
  , currentTimestamp
  , validateText
  , sanitizeInput

    -- * Type Utilities
  , showMEUType
  , parseMEUType
  , validateTypeCompatibility

    -- * ID Generation
  , generateTripletId
  , generateTaskId
  , generateTestId
  , generateValueId
  , generateDSLPrimitiveId

    -- * Error Utilities
  , wrapError
  , chainErrors
  , logError

    -- * Performance Utilities
  , measureTime
  , profileFunction
  , memoryUsage
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import System.CPUTime (getCPUTime)

import MEU.Core.Types
import MEU.DSL.Types (DSLPrimitiveId(..))

-- | Generate a new UUID
generateUUID :: IO UUID
generateUUID = UUID.nextRandom

-- | Get current timestamp
currentTimestamp :: IO Timestamp
currentTimestamp = Timestamp <$> getCurrentTime

-- | Validate text input
validateText :: Text -> Either MEUError Text
validateText text
  | T.null text = Left (UnknownError "Empty text not allowed")
  | T.length text > 10000 = Left (UnknownError "Text too long")
  | otherwise = Right text

-- | Sanitize user input
sanitizeInput :: Text -> Text
sanitizeInput = T.filter (/= '\0') . T.take 1000

-- | Show MEU type as text
showMEUType :: MEUType -> Text
showMEUType meuType = case meuType of
  BaseType bt -> showBaseType bt
  ProductType t1 t2 -> "(" <> showMEUType t1 <> " × " <> showMEUType t2 <> ")"
  SumType t1 t2 -> "(" <> showMEUType t1 <> " + " <> showMEUType t2 <> ")"
  FunctionType t1 t2 -> "(" <> showMEUType t1 <> " → " <> showMEUType t2 <> ")"
  UnitType -> "⊤"
  BottomType -> "⊥"
  NegationType t -> "¬" <> showMEUType t

-- | Show base type as text
showBaseType :: BaseType -> Text
showBaseType bt = case bt of
  StringType -> "String"
  IntType -> "Int"
  BoolType -> "Bool"
  FloatType -> "Float"
  ModelType -> "Model"
  ExecutionType -> "Execution"
  UpdateType -> "Update"
  ValueType -> "Value"
  TestType -> "Test"
  VerifierType -> "Verifier"
  CustomType name -> name

-- | Parse MEU type from text
parseMEUType :: Text -> Either MEUError MEUType
parseMEUType text = case text of
  "String" -> Right (BaseType StringType)
  "Int" -> Right (BaseType IntType)
  "Bool" -> Right (BaseType BoolType)
  "Float" -> Right (BaseType FloatType)
  "Model" -> Right (BaseType ModelType)
  "Execution" -> Right (BaseType ExecutionType)
  "Update" -> Right (BaseType UpdateType)
  "Value" -> Right (BaseType ValueType)
  "Test" -> Right (BaseType TestType)
  "Verifier" -> Right (BaseType VerifierType)
  "⊤" -> Right UnitType
  "⊥" -> Right BottomType
  _ -> Right (BaseType (CustomType text))

-- | Validate type compatibility
validateTypeCompatibility :: MEUType -> MEUType -> Bool
validateTypeCompatibility t1 t2 = case (t1, t2) of
  (UnitType, UnitType) -> True
  (BottomType, _) -> True
  (_, BottomType) -> False
  (BaseType bt1, BaseType bt2) -> bt1 == bt2
  (ProductType a1 b1, ProductType a2 b2) ->
    validateTypeCompatibility a1 a2 && validateTypeCompatibility b1 b2
  (SumType a1 b1, SumType a2 b2) ->
    validateTypeCompatibility a1 a2 && validateTypeCompatibility b1 b2
  (FunctionType a1 b1, FunctionType a2 b2) ->
    validateTypeCompatibility a2 a1 && validateTypeCompatibility b1 b2 -- Contravariant in input
  (NegationType t1', NegationType t2') -> validateTypeCompatibility t1' t2'
  _ -> False

-- | Generate triplet ID
generateTripletId :: IO TripletId
generateTripletId = TripletId <$> generateUUID

-- | Generate task ID
generateTaskId :: IO TaskId
generateTaskId = TaskId <$> generateUUID

-- | Generate test ID
generateTestId :: IO TestId
generateTestId = TestId <$> generateUUID

-- | Generate value ID
generateValueId :: IO ValueId
generateValueId = ValueId <$> generateUUID

-- | Generate DSL primitive ID
generateDSLPrimitiveId :: IO DSLPrimitiveId
generateDSLPrimitiveId = DSLPrimitiveId <$> generateUUID

-- | Wrap error with context
wrapError :: Text -> MEUError -> MEUError
wrapError context err = case err of
  TypeMismatchError expected actual msg -> TypeMismatchError expected actual (context <> ": " <> msg)
  DomainInclusionError msg -> DomainInclusionError (context <> ": " <> msg)
  AxiomInconsistencyError msg -> AxiomInconsistencyError (context <> ": " <> msg)
  RefinementError msg -> RefinementError (context <> ": " <> msg)
  CoarseningError msg -> CoarseningError (context <> ": " <> msg)
  RegistryError msg -> RegistryError (context <> ": " <> msg)
  IOError msg -> IOError (context <> ": " <> msg)
  ExecutionError msg -> ExecutionError (context <> ": " <> msg)
  SUDExecutionError msg -> SUDExecutionError (context <> ": " <> msg)
  CompositionError msg -> CompositionError (context <> ": " <> msg)
  FunctionNotFoundError msg -> FunctionNotFoundError (context <> ": " <> msg)
  ValidationError msg -> ValidationError (context <> ": " <> msg)
  UnknownError msg -> UnknownError (context <> ": " <> msg)

-- | Chain multiple errors
chainErrors :: [MEUError] -> MEUError
chainErrors [] = UnknownError "No errors provided"
chainErrors [single] = single
chainErrors errors = UnknownError (T.intercalate "; " (map showError errors))

-- | Show error as text
showError :: MEUError -> Text
showError err = case err of
  TypeMismatchError expected actual msg -> "Type mismatch (expected " <> T.pack (show expected) <> ", got " <> T.pack (show actual) <> "): " <> msg
  DomainInclusionError msg -> "Domain inclusion: " <> msg
  AxiomInconsistencyError msg -> "Axiom inconsistency: " <> msg
  RefinementError msg -> "Refinement: " <> msg
  CoarseningError msg -> "Coarsening: " <> msg
  RegistryError msg -> "Registry: " <> msg
  IOError msg -> "IO: " <> msg
  ExecutionError msg -> "Execution: " <> msg
  SUDExecutionError msg -> "SUD Execution: " <> msg
  CompositionError msg -> "Composition: " <> msg
  FunctionNotFoundError msg -> "Function not found: " <> msg
  ValidationError msg -> "Validation: " <> msg
  UnknownError msg -> "Unknown: " <> msg

-- | Log error (simplified implementation)
logError :: MEUError -> IO ()
logError err = putStrLn $ "ERROR: " <> T.unpack (showError err)

-- | Measure execution time
measureTime :: IO a -> IO (a, Double)
measureTime action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let duration = fromIntegral (end - start) / (10^12) -- Convert to seconds
  return (result, duration)

-- | Profile function execution
profileFunction :: Text -> IO a -> IO a
profileFunction name action = do
  (result, duration) <- measureTime action
  putStrLn $ T.unpack name <> " took " <> show duration <> " seconds"
  return result

-- | Get memory usage (simplified)
memoryUsage :: IO Int
memoryUsage = return 0 -- Placeholder - would use proper memory measurement

-- | Safe division
safeDivide :: Double -> Double -> Either MEUError Double
safeDivide _ 0 = Left (UnknownError "Division by zero")
safeDivide x y = Right (x / y)

-- | Safe list operations
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- | Retry operation with exponential backoff
retryWithBackoff :: Int -> IO (Either e a) -> IO (Either e a)
retryWithBackoff 0 action = action
retryWithBackoff n action = do
  result <- action
  case result of
    Left _ -> do
      threadDelay (1000000 * (2 ^ (3 - n))) -- Exponential backoff
      retryWithBackoff (n - 1) action
    Right success -> return (Right success)

-- | Batch process items
batchProcess :: Int -> [a] -> (a -> IO b) -> IO [b]
batchProcess batchSize items processor = do
  let batches = chunksOf batchSize items
  concat <$> mapM (mapM processor) batches

-- | Split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Safe text conversion
safeTextShow :: Show a => a -> Text
safeTextShow = T.pack . show

-- | Validate UUID format
validateUUID :: Text -> Either MEUError UUID
validateUUID text = case UUID.fromText text of
  Just uuid -> Right uuid
  Nothing -> Left (UnknownError "Invalid UUID format")

-- | Generate random text
generateRandomText :: Int -> IO Text
generateRandomText len = do
  uuid <- generateUUID
  let uuidText = T.take len (T.filter (/= '-') (T.pack (show uuid)))
  return uuidText

