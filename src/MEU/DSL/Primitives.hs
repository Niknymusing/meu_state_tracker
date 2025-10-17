{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module MEU.DSL.Primitives
  ( -- * DSL Primitive Operations
    createPrimitive
  , executePrimitive
  , composePrimitives
  , validatePrimitive

    -- * Standard Primitives
  , identityPrimitive
  , testPrimitive
  , compressionPrimitive
  , validationPrimitive

    -- * Primitive Categories
  , ModelPrimitives(..)
  , ExecutionPrimitives(..)
  , UpdatePrimitives(..)

    -- * Primitive Execution
  , PrimitiveExecutor(..)
  , PrimitiveExecutionEnvironment(..)
  , executePrimitiveInEnvironment
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID (nil)
import GHC.Generics (Generic)

import MEU.Core.Types
import MEU.DSL.Types (TypedValue(..), ValueContent(..), DSLPrimitive(..), DSLFunction(..), ExecutionContext(..), ExecutionResult(..), ValueId(..), DSLPrimitiveId(..))

-- Helper functions
getCurrentTimeStamp :: IO Timestamp
getCurrentTimeStamp = return $ Timestamp undefined

generatePrimitiveId :: Text -> DSLPrimitiveId
generatePrimitiveId _name = DSLPrimitiveId nil

-- | Model domain primitives
data ModelPrimitives = ModelPrimitives
  { specificationPrimitives :: [DSLPrimitive]
  , compressionPrimitives :: [DSLPrimitive]
  , templatePrimitives :: [DSLPrimitive]
  , workflowPrimitives :: [DSLPrimitive]
  } deriving (Eq, Show, Generic)

-- | Execution domain primitives
data ExecutionPrimitives = ExecutionPrimitives
  { deploymentPrimitives :: [DSLPrimitive]
  , testingPrimitives :: [DSLPrimitive]
  , monitoringPrimitives :: [DSLPrimitive]
  , resourcePrimitives :: [DSLPrimitive]
  } deriving (Eq, Show, Generic)

-- | Update domain primitives
data UpdatePrimitives = UpdatePrimitives
  { evaluationPrimitives :: [DSLPrimitive]
  , verificationPrimitives :: [DSLPrimitive]
  , updatePrimitives :: [DSLPrimitive]
  , configurationPrimitives :: [DSLPrimitive]
  } deriving (Eq, Show, Generic)

-- | Primitive executor
data PrimitiveExecutor = PrimitiveExecutor
  { executorId :: Text
  , executorEnvironment :: PrimitiveExecutionEnvironment
  , executorCapabilities :: [Text]
  , executorActive :: Bool
  } deriving (Eq, Show, Generic)

-- | Execution environment for primitives
data PrimitiveExecutionEnvironment = PrimitiveExecutionEnvironment
  { envTriplet :: TripletId
  , envDomain :: DomainId
  , envVariables :: Map Text TypedValue
  , envConfiguration :: Map Text Text
  , envResources :: Map Text Text
  } deriving (Eq, Show, Generic)

-- | Create new DSL primitive
createPrimitive :: Text -> Text -> DomainType -> [MEUType] -> MEUType -> DSLFunction -> IO DSLPrimitive
createPrimitive name _description domain inputTypes outputType function = do
  let primitiveId = generatePrimitiveId name
  return DSLPrimitive
    { dslPrimitiveId = primitiveId
    , dslPrimitiveName = name
    , dslPrimitiveDomain = domain
    , dslPrimitiveInputTypes = inputTypes
    , dslPrimitiveOutputType = outputType
    , dslPrimitiveFunction = function
    , dslPrimitiveIsIdentity = False
    }

-- | Execute DSL primitive
executePrimitive :: DSLPrimitive -> TypedValue -> ExecutionContext -> IO (Either MEUError ExecutionResult)
executePrimitive primitive input context = do
  now <- getCurrentTimeStamp
  let function = dslPrimitiveFunction primitive
  -- For now, simulate execution based on function implementation
  if dslPrimitiveIsIdentity primitive
    then return $ Right $ ExecutionResult (Right input) context Map.empty 0.0
    else do
      -- Simulate function execution based on implementation text
      let result = case functionImplementation function of
            impl | "identity" `T.isInfixOf` impl -> Right input
                 | "test" `T.isInfixOf` impl -> Right input -- Test functions pass through
                 | otherwise -> Right input -- Default behavior
      return $ Right $ ExecutionResult result context Map.empty 1.0

-- | Compose multiple primitives
composePrimitives :: [DSLPrimitive] -> IO (Either MEUError DSLPrimitive)
composePrimitives primitives = do
  case primitives of
    [] -> return $ Left $ ExecutionError "Cannot compose empty list of primitives"
    [single] -> return $ Right single
    (first:rest) -> do
      -- Validate composition compatibility
      case validateComposition primitives of
        Right () -> do
          composite <- createCompositePrimitive primitives
          return $ Right composite
        Left err -> return $ Left err

-- | Validate primitive
validatePrimitive :: DSLPrimitive -> IO (Either MEUError ())
validatePrimitive primitive = do
  -- Validate input and output types
  case validatePrimitiveTypes primitive of
    Right () -> return $ Right ()
    Left err -> return $ Left err

validatePrimitiveTypes :: DSLPrimitive -> Either MEUError ()
validatePrimitiveTypes primitive = do
  mapM_ validateMEUType (dslPrimitiveInputTypes primitive)
  validateMEUType (dslPrimitiveOutputType primitive)
  Right ()

-- | Execute primitive in specific environment
executePrimitiveInEnvironment :: DSLPrimitive -> TypedValue -> PrimitiveExecutionEnvironment -> IO (Either MEUError ExecutionResult)
executePrimitiveInEnvironment primitive input env = do
  let context = environmentToContext env
  executePrimitive primitive input context

-- Standard primitives

-- | Identity primitive function
identityPrimitive :: DomainType -> IO DSLPrimitive
identityPrimitive domain = do
  let function = DSLFunction
        { functionId = "identity_fn"
        , functionName = "identity"
        , functionImplementation = "identity function implementation"
        }
  createPrimitive "identity" "Identity function" domain [UnitType] UnitType function

-- | Test primitive function
testPrimitive :: TestId -> DomainType -> IO DSLPrimitive
testPrimitive testId domain = do
  let function = DSLFunction
        { functionId = T.pack $ "test_fn_" ++ show testId
        , functionName = "test"
        , functionImplementation = "test execution function implementation"
        }
  createPrimitive "test" "Test execution function" domain [UnitType] (BaseType BoolType) function

-- | Compression primitive function
compressionPrimitive :: DomainType -> IO DSLPrimitive
compressionPrimitive domain = do
  let function = DSLFunction
        { functionId = "compress_fn"
        , functionName = "compress"
        , functionImplementation = "data compression function implementation"
        }
  createPrimitive "compress" "Data compression function" domain [UnitType] UnitType function

-- | Validation primitive function
validationPrimitive :: DomainType -> IO DSLPrimitive
validationPrimitive domain = do
  let function = DSLFunction
        { functionId = "validate_fn"
        , functionName = "validate"
        , functionImplementation = "value validation function implementation"
        }
  createPrimitive "validate" "Value validation function" domain [UnitType] (BaseType BoolType) function

-- Composition validation functions

validateComposition :: [DSLPrimitive] -> Either MEUError ()
validateComposition primitives = do
  -- Check that output types match input types in sequence
  case checkTypeChain primitives of
    True -> Right ()
    False -> Left $ ExecutionError "Primitive types are not composable"

createCompositePrimitive :: [DSLPrimitive] -> IO DSLPrimitive
createCompositePrimitive primitives = do
  let ids = map dslPrimitiveId primitives
      name = "composite_" <> T.intercalate "_" (map dslPrimitiveName primitives)
      description = "Composite of: " <> T.intercalate ", " (map dslPrimitiveName primitives)
      -- Compute composite types
      (inputTypes, outputType) = computeCompositeTypes primitives
      domain = dslPrimitiveDomain (head primitives)
      function = DSLFunction
        { functionId = "composite_" <> T.intercalate "_" (map (\(DSLPrimitiveId uuid) -> T.take 8 $ T.pack $ show uuid) ids)
        , functionName = name
        , functionImplementation = "composite function of: " <> description
        }

  createPrimitive name description domain inputTypes outputType function

validateTypeSignature :: TypeSignature -> Either MEUError ()
validateTypeSignature signature = do
  -- Validate input and output types
  mapM_ validateMEUType (inputTypes signature)
  validateMEUType (outputType signature)
  Right ()

environmentToContext :: PrimitiveExecutionEnvironment -> ExecutionContext
environmentToContext env = ExecutionContext
  { contextTripletId = envTriplet env
  , contextDomainId = envDomain env
  , contextEnvironment = Map.map (const "value") (envVariables env)
  , contextTimestamp = Timestamp undefined
  }

checkTypeChain :: [DSLPrimitive] -> Bool
checkTypeChain [] = True
checkTypeChain [_] = True
checkTypeChain (p1:p2:rest) =
  let out1 = dslPrimitiveOutputType p1
      in2 = case dslPrimitiveInputTypes p2 of
              [] -> UnitType
              (t:_) -> t
  in out1 == in2 && checkTypeChain (p2:rest)

computeCompositeTypes :: [DSLPrimitive] -> ([MEUType], MEUType)
computeCompositeTypes [] = ([], UnitType)
computeCompositeTypes primitives =
  let firstInputs = dslPrimitiveInputTypes (head primitives)
      lastOutput = dslPrimitiveOutputType (last primitives)
  in (firstInputs, lastOutput)

validateMEUType :: MEUType -> Either MEUError ()
validateMEUType meuType = case meuType of
  BaseType _ -> Right ()
  ProductType t1 t2 -> do
    validateMEUType t1
    validateMEUType t2
  SumType t1 t2 -> do
    validateMEUType t1
    validateMEUType t2
  FunctionType t1 t2 -> do
    validateMEUType t1
    validateMEUType t2
  UnitType -> Right ()
  BottomType -> Right ()
  NegationType t -> validateMEUType t

