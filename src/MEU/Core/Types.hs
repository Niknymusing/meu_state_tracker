{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module MEU.Core.Types
  ( -- * Core Type System
    MEUType(..)
  , BaseType(..)
  , TypeConstructor(..)
  , TypeSignature(..)

    -- * Domain Types
  , DomainId(..)
  , Domain(..)

    -- * MEU Triplet Identifiers
  , TripletId(..)
  , TripletType(..)

    -- * Task and Ticket Types
  , TaskId(..)
  , TicketId(..)
  , Task(..)
  , Ticket(..)

    -- * Acceptance Criteria
  , AcceptanceCriteria(..)
  , GeometricFormula(..)
  , LogicalRelation(..)
  , Axiom(..)

    -- * Test Types
  , TestId(..)
  , TestType(..)
  , TestStatus(..)
  , Test(..)

    -- * Utility Types
  , Timestamp(..)
  , MEUError(..)
  , Version(..)

    -- * Triplet State Types
  , TripletMetadata(..)
  , TripletRelations(..)
  , TripletStatus(..)
  , ModelState(..)
  , ExecutionState(..)
  , UpdateState(..)
  , SystemState(..)

    -- * Additional Core Types
  , ValueId(..)
  , TypedValue(..)
  , ValueContent(..)
  , getValueType
  , ExecutionEnvironment(..)
  , LoggingConfiguration(..)
  , ResourceAllocation(..)
  , ProjectSpecification(..)
  , MEUSystem(..)

    -- * Complete MEU Triplet Definition
  , MEUTriplet(..)
  , DomainType(..)
  , DataflowArrow(..)
  , ArrowType(..)
  , ArrowId(..)
  , DSLPrimitive(..)
  , DSLPrimitiveId(..)
  , DSLFunction(..)
  , InclusionType(..)
  , GeometricTheory(..)

    -- * Enhanced GADT-based MEU Triplet with Effectful Stacking
  , MEUTripletEff(..)
  , DomainState(..)
  , AdjointPair(..)
  , InclusionOp(..)
  , DataflowArrowCollection(..)
  , EnhancedSUDPointer(..)
  , EnhancedSUDEndpoint(..)
  , MEUComputation

    -- * Phantom Types for Capabilities
  , Instantiated
  , NotInstantiated
  , Validated
  , NotValidated
  , Executable
  , NotExecutable
  , DomainValue
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Effectful
import Effectful.State.Static.Local
import Effectful.Error.Static
import Effectful.Reader.Static

-- | Core type system for MEU framework DSL
data MEUType
  = BaseType BaseType
  | ProductType MEUType MEUType        -- (×, ∧)
  | SumType MEUType MEUType           -- (+, ∨)
  | FunctionType MEUType MEUType      -- (→, ⇒)
  | UnitType                          -- (1, ⊤)
  | BottomType                        -- (0, ⊥)
  | NegationType MEUType              -- (¬A := A → ⊥)
  deriving (Eq, Show, Ord, Generic)

-- | Base types for MEU framework domains
data BaseType
  = StringType
  | IntType
  | BoolType
  | FloatType
  | ModelType
  | ExecutionType
  | UpdateType
  | ValueType
  | TestType
  | VerifierType
  | CustomType Text
  deriving (Eq, Show, Ord, Generic)

-- | Type constructors for building complex types
data TypeConstructor
  = Product
  | Sum
  | Function
  | Unit
  | Bottom
  | Negation
  deriving (Eq, Show, Generic)

-- | Type signature for DSL primitives
data TypeSignature = TypeSignature
  { inputTypes :: [MEUType]
  , outputType :: MEUType
  , constraints :: [Text]
  } deriving (Eq, Show, Generic)

-- | Domain identifier for M, E, U domains (legacy - use DomainType instead)
data DomainId
  = ModelDomainId
  | ExecutionDomainId
  | UpdateDomainId
  deriving (Eq, Show, Ord, Generic)

-- | Unique identifier for MEU triplets
newtype TripletId = TripletId UUID
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Read)

-- | Type of MEU triplet in the system tree
data TripletType
  = SourceTriplet     -- τ0 - root triplet from project specification
  | BranchTriplet     -- top-down managing subtriplets
  | LeafTriplet       -- bottom-up operating in execution environment
  deriving (Eq, Show, Generic)

-- | Task identifier
newtype TaskId = TaskId UUID
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Read)

-- | Ticket identifier
newtype TicketId = TicketId UUID
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Read)

-- | Task specification
data Task = Task
  { taskId :: TaskId
  , taskDescription :: Text
  , taskSpecification :: Text
  , taskDependencies :: [TaskId]
  , taskStatus :: TestStatus
  , taskCreated :: Timestamp
  , taskUpdated :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Ticket for implementation within MEU system
data Ticket = Ticket
  { ticketId :: TicketId
  , ticketTask :: TaskId
  , ticketTriplet :: TripletId
  , ticketSpecification :: Text
  , ticketAcceptanceCriteria :: [AcceptanceCriteria]
  , ticketStatus :: TestStatus
  , ticketCreated :: Timestamp
  , ticketUpdated :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Acceptance criteria for development lifecycle verification
data AcceptanceCriteria = AcceptanceCriteria
  { criteriaId :: Text
  , criteriaDescription :: Text
  , criteriaFormulas :: [GeometricFormula]
  , criteriaAxioms :: [Axiom]
  , criteriaTestIds :: [TestId]
  } deriving (Eq, Show, Generic)

-- | Geometric logic formula for first-order geometric theory
data GeometricFormula = GeometricFormula
  { formulaVariables :: [(Text, MEUType)]
  , formulaConstraints :: [LogicalRelation]
  , formulaConclusion :: LogicalRelation
  } deriving (Eq, Show, Generic)

-- | Logical relations for geometric formulas
data LogicalRelation
  = Equals Text Text
  | GreaterThan Text Text
  | LessThan Text Text
  | GreaterEquals Text Text
  | LessEquals Text Text
  | And LogicalRelation LogicalRelation
  | Or LogicalRelation LogicalRelation
  | Not LogicalRelation
  | Exists Text MEUType LogicalRelation
  | ForAll Text MEUType LogicalRelation
  | Truth
  | Falsehood
  | CustomRelation Text [Text]
  deriving (Eq, Show, Generic)

-- | Axiom as sequent between formulas
data Axiom = Axiom
  { axiomId :: Text
  , axiomPremise :: GeometricFormula
  , axiomConclusion :: GeometricFormula
  , axiomDescription :: Text
  } deriving (Eq, Show, Generic)

-- | Test identifier
newtype TestId = TestId UUID
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Read)

-- | Test hierarchy pyramid according to MEU specification
data TestType
  = TypeTest              -- (i) Type tests (Value-validating, unit test)
  | ExecutionOperatorTest -- (ii) Execution Operator tests (unit test, integration test)
  | FormulaTest          -- (iii) Formulas (Acceptance criteria tests)
  | AxiomCoherenceTest   -- (iv) Axioms coherence tests (SMT verification)
  | IntegrationMergeTest -- Integration/Merge Operator tests for value combination
  deriving (Eq, Show, Ord, Generic)

-- | Test execution status
data TestStatus
  = Pending
  | InProgress
  | Passed
  | Failed Text
  | Blocked Text
  deriving (Eq, Show, Generic)

-- | Test specification and execution according to MEU hierarchy
data Test = Test
  { testId :: TestId
  , testType :: TestType
  , testDescription :: Text

  -- MEU system integration metadata
  , testTripletId :: TripletId
  , testDomainId :: DomainId
  , testConnectedTriplets :: Set TripletId -- For interconnected set tests

  -- Type system integration
  , testInputSignature :: TypeSignature
  , testOutputSignature :: TypeSignature
  , testValidatedTypes :: Set MEUType -- For TypeTest

  -- DSL primitive integration
  , testDSLPrimitives :: Set DSLPrimitiveId -- DSL functions used in test
  , testIsValueGenerator :: Bool -- True for tests that produce typed values
  , testGeneratedValueTypes :: Set MEUType

  -- Execution metadata
  , testPointer :: Text -- Pointer to executable test in WS state tracker
  , testExecutionEndpoint :: Maybe Text -- External SUD endpoint
  , testVerifiers :: Set Text -- Verifier functions for evaluation

  -- Status and results
  , testStatus :: TestStatus
  , testResults :: Maybe Text -- Execution results
  , testCreated :: Timestamp
  , testLastRun :: Maybe Timestamp
  , testError :: Maybe Text
  } deriving (Eq, Show, Generic)

-- | Timestamp wrapper
newtype Timestamp = Timestamp UTCTime
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Read)

-- | Error types for MEU system
data MEUError
  = TypeMismatchError MEUType MEUType Text  -- Expected, Actual, Context
  | DomainInclusionError Text
  | AxiomInconsistencyError Text
  | RefinementError Text
  | CoarseningError Text
  | RegistryError Text
  | IOError Text
  | ValidationError Text
  | ExecutionError Text
  | SUDExecutionError Text
  | CompositionError Text
  | FunctionNotFoundError Text
  | UnknownError Text
  deriving (Eq, Show, Generic)

-- | Version information
data Version = Version Int Int Int
  deriving (Eq, Show, Ord, Generic)

-- | Value identifier
newtype ValueId = ValueId UUID
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Read)

-- | Content of a typed value
data ValueContent
  = StringValue Text
  | IntValue Int
  | BoolValue Bool
  | FloatValue Double
  | ListValue [TypedValue]
  | RecordValue [(Text, TypedValue)]
  | CompositeValue [ValueContent]
  | NullValue
  deriving (Eq, Show, Generic)

-- | Typed value for MEU domains
data TypedValue = TypedValue
  { valueType :: MEUType
  , valueContent :: ValueContent
  } deriving (Eq, Show, Generic)

-- | Extract the type from a TypedValue
getValueType :: TypedValue -> MEUType
getValueType = valueType

-- | Triplet metadata
data TripletMetadata = TripletMetadata
  { tripletCreated :: Timestamp
  , tripletUpdated :: Timestamp
  , tripletVersion :: Version
  , tripletDescription :: Text
  , tripletParentId :: Maybe TripletId
  , tripletChildIds :: [TripletId]
  , tripletTags :: [Text]
  , tripletDepth :: Int
  } deriving (Eq, Show, Generic)

-- | Triplet relations management
data TripletRelations = TripletRelations
  { relationsParent :: Maybe TripletId
  , relationsChildren :: [TripletId]
  , relationsSiblings :: [TripletId]
  } deriving (Eq, Show, Generic)

-- | Triplet lifecycle status
data TripletStatus
  = Active
  | Inactive
  | Archived
  | TripletFailed Text
  deriving (Eq, Show, Generic)

-- | Model domain state
data ModelState = ModelState
  { modelSpecifications :: Map Text Text -- TODO: Define proper specification types
  , modelDSLPrimitives :: Map Text Text   -- TODO: Define proper DSL primitives
  , modelTypes :: Map Text MEUType
  , modelValues :: Map ValueId TypedValue
  , modelTests :: Map TestId Test
  } deriving (Eq, Show, Generic)

-- | Execution environment configuration
data ExecutionEnvironment = ExecutionEnvironment
  { envName :: Text
  , envDescription :: Text
  , envConfiguration :: Text -- JSON configuration
  } deriving (Eq, Show, Generic)

-- | Logging configuration
data LoggingConfiguration = LoggingConfiguration
  { logLevel :: Text
  , logOutput :: Text
  , logFormat :: Text
  } deriving (Eq, Show, Generic)

-- | Resource allocation settings
data ResourceAllocation = ResourceAllocation
  { allocCores :: Int
  , allocMemoryMB :: Int
  , allocTimeoutMs :: Int
  } deriving (Eq, Show, Generic)

-- | Execution domain state
data ExecutionState = ExecutionState
  { execEnvironment :: ExecutionEnvironment
  , execDeployedModels :: [Text] -- TODO: Define proper deployment models
  , execLoggingConfig :: LoggingConfiguration
  , execResourceAllocation :: ResourceAllocation
  , execFeedbackChannels :: [Text] -- TODO: Define proper feedback channels
  } deriving (Eq, Show, Generic)

-- | Update domain state
data UpdateState = UpdateState
  { updateEvaluators :: [Text]     -- TODO: Define proper evaluator types
  , updateVerifiers :: [Text]      -- TODO: Define proper verifier types
  , updateGeometricTheory :: Text  -- TODO: Define proper geometric theory type
  , updateAcceptanceCriteria :: [AcceptanceCriteria]
  , updatePolicies :: [Text]       -- TODO: Define proper policy types
  } deriving (Eq, Show, Generic)

-- | System state for persistence
data SystemState = SystemState
  { systemTriplets :: [(TripletId, Text)] -- Simplified triplet storage
  , systemMetadata :: [(Text, Text)]      -- Key-value metadata
  , systemConfig :: [(Text, Text)]        -- Configuration settings
  } deriving (Eq, Show, Generic)

-- DataflowCollection is defined in MEU.DSL.Types

-- | Project specification for initialization
data ProjectSpecification = ProjectSpecification
  { projectName :: Text
  , projectDescription :: Text
  , projectVersion :: Version
  } deriving (Eq, Show, Generic)

-- | MEU System containing triplets and registries
data MEUSystem = MEUSystem
  { meuSystemId :: Text
  , meuSystemRootTriplet :: Maybe TripletId
  , meuSystemTriplets :: Map TripletId Text -- Simplified for now
  , meuSystemMetadata :: Map Text Text
  , meuSystemConfig :: Map Text Text
  , meuSystemCreated :: Timestamp
  } deriving (Eq, Show, Generic)

-- ============================================================================
-- Complete MEU Triplet Definition According to Specification
-- ============================================================================

-- | Domain types for MEU triplet τ = (M_τ, E_τ, U_τ)
data DomainType = ModelDomain | ExecuteDomain | UpdateDomain
  deriving (Eq, Show, Ord, Generic)

-- | MEU triplet domain with state transformation rules Λτ|D
data Domain = Domain
  { domainType :: DomainType
  , domainState :: Text -- TODO: Replace with proper domain-specific state types
  , domainDSLPrimitives :: Set DSLPrimitiveId
  , domainTypes :: Set MEUType
  , domainStateTransformRules :: Map Text DSLFunction -- Λτ|M, Λτ|E, or Λτ|U
  } deriving (Eq, Show, Generic)

-- | Arrow types for dataflow between domains
data ArrowType
  = I_Arrow    -- I: M→E (model deployment)
  | IStar_Arrow -- I*: E→M (configuration feedback)
  | O_Arrow    -- O: E→U (execution logging)
  | OStar_Arrow -- O*: U→E (environment adaptation)
  | R_Arrow    -- R: U→M (verification results)
  | RStar_Arrow -- R*: M→U (criteria deployment)
  deriving (Eq, Show, Ord, Generic)

-- | Unique arrow identifier
newtype ArrowId = ArrowId UUID
  deriving (Eq, Show, Ord, Generic)

-- | Dataflow arrow implementing f_{D1,D2} = (A, A*)
data DataflowArrow = DataflowArrow
  { arrowId :: ArrowId
  , arrowType :: ArrowType
  , arrowSourceDomain :: DomainType
  , arrowTargetDomain :: DomainType
  , arrowSourceType :: MEUType
  , arrowTargetType :: MEUType
  , arrowFunction :: DSLFunction
  , arrowIdentityMap :: Bool -- True if this is the required identity map n_{D1,D2}
  } deriving (Eq, Show, Generic)

-- | DSL primitive identifier
newtype DSLPrimitiveId = DSLPrimitiveId UUID
  deriving (Eq, Show, Ord, Generic)

-- | DSL primitive function within domain D
data DSLPrimitive = DSLPrimitive
  { dslPrimitiveId :: DSLPrimitiveId
  , dslPrimitiveName :: Text
  , dslPrimitiveDomain :: DomainType
  , dslPrimitiveInputTypes :: [MEUType]
  , dslPrimitiveOutputType :: MEUType
  , dslPrimitiveFunction :: DSLFunction
  , dslPrimitiveIsIdentity :: Bool -- True if this is Id(τ|D)
  } deriving (Eq, Show, Generic)

-- | DSL function implementation (placeholder for actual function)
data DSLFunction = DSLFunction
  { functionId :: Text
  , functionName :: Text
  , functionImplementation :: Text -- TODO: Replace with actual function type
  } deriving (Eq, Show, Generic)

-- ============================================================================
-- GADT-based MEU Triplet with Phantom Types and Effectful Monadic Stacking
-- ============================================================================

-- | Phantom types for MEU triplet capabilities
data Instantiated
data NotInstantiated
data Validated
data NotValidated
data Executable
data NotExecutable

-- | Effectful computation type for MEU operations
type MEUComputation es a = Eff es a

-- | Domain state with effectful monadic stacking
data DomainState (d :: DomainType) (capability :: Type) es where
  ModelDomainState ::
    { modelDomainSpecs :: Map Text Text
    , modelDomainDSLOps :: Map DSLPrimitiveId (MEUComputation es TypedValue)
    , modelDomainTypes :: Set MEUType
    , modelDomainInheritanceChain :: [TripletId]
    } -> DomainState 'ModelDomain capability es

  ExecuteDomainState ::
    { executeDomainEnvironment :: ExecutionEnvironment
    , executeDomainDeployedOps :: Map DSLPrimitiveId (MEUComputation es TypedValue)
    , executeDomainResourceAlloc :: ResourceAllocation
    , executeDomainInheritanceChain :: [TripletId]
    } -> DomainState 'ExecuteDomain capability es

  UpdateDomainState ::
    { updateDomainVerifiers :: Map Text (MEUComputation es Bool)
    , updateDomainGeometric :: GeometricTheory
    , updateDomainCriteria :: [AcceptanceCriteria]
    , updateDomainInheritanceChain :: [TripletId]
    } -> DomainState 'UpdateDomain capability es

-- | Type family for extracting value type from DomainState
type family DomainValue (d :: DomainType) :: Type where
  DomainValue 'ModelDomain = TypedValue
  DomainValue 'ExecuteDomain = TypedValue
  DomainValue 'UpdateDomain = Bool

-- | Adjoint functor pair for dataflow arrows as per MEU specification
data AdjointPair (source :: DomainType) (target :: DomainType) (es :: [Effect]) = AdjointPair
  { leftAdjoint :: forall capability.
      DomainState source capability es ->
      MEUComputation es (DomainState target capability es)  -- Forward arrow
  , rightAdjoint :: forall capability.
      DomainState target capability es ->
      MEUComputation es (DomainState source capability es) -- Backward arrow
  , unit :: forall capability a.
      a -> MEUComputation es (DomainState target capability es)      -- Unit of adjunction
  , counit :: forall capability a.
      MEUComputation es (DomainState source capability es) -> MEUComputation es a -- Counit
  , identityMap :: forall a. a -> MEUComputation es a       -- Required identity n_{D1,D2}
  }

-- | Domain inclusion operation implementing ~D~> as per specification
data InclusionOp (d :: DomainType) es = InclusionOp
  { includeChild :: forall capability.
      TripletId ->
      DomainState d capability es ->
      MEUComputation es (DomainState d capability es)
  , compressContext ::
      [TripletId] ->
      MEUComputation es (Map Text Text) -- Compressed specifications
  , validateInclusion ::
      TripletId ->
      MEUComputation es (Either MEUError ())
  }

-- | Dataflow arrow collection with effectful computations
data DataflowArrowCollection es = DataflowArrowCollection
  { -- f_{M,E} = (I: M→E, I*: E→M)
    arrowME :: AdjointPair 'ModelDomain 'ExecuteDomain es
  , arrowEM :: AdjointPair 'ExecuteDomain 'ModelDomain es

    -- f_{E,U} = (O: E→U, O*: U→E)
  , arrowEU :: AdjointPair 'ExecuteDomain 'UpdateDomain es
  , arrowUE :: AdjointPair 'UpdateDomain 'ExecuteDomain es

    -- f_{U,M} = (R: U→M, R*: M→U)
  , arrowUM :: AdjointPair 'UpdateDomain 'ModelDomain es
  , arrowMU :: AdjointPair 'ModelDomain 'UpdateDomain es
  }

-- | Enhanced SUD Endpoint for effectful external function execution
data EnhancedSUDEndpoint = EnhancedSUDEndpoint
  { enhancedEndpointType :: Text -- "api", "function", "process", etc.
  , enhancedEndpointAddress :: Text
  , enhancedEndpointAuth :: Maybe Text
  , enhancedEndpointTimeout :: Int -- milliseconds
  } deriving (Eq, Show, Generic)

-- | Enhanced SUD Pointer with effectful execution
data EnhancedSUDPointer = EnhancedSUDPointer
  { enhancedPointerId :: Text
  , enhancedEndpoint :: EnhancedSUDEndpoint
  , enhancedSignature :: TypeSignature
  , enhancedExecutor :: forall es. (IOE :> es) => [TypedValue] -> MEUComputation es (Either MEUError TypedValue)
  , enhancedMetadata :: Map Text Text
  , enhancedActive :: Bool
  }

-- | GADT-based MEU Triplet with phantom types and effectful stacking
data MEUTripletEff
  (instantiated :: Type)
  (validated :: Type)
  (executable :: Type)
  (es :: [Effect]) where

  -- Source triplet from project specification
  SourceTripletEff ::
    { sourceTripletId :: TripletId
    , sourceModelDomain :: DomainState 'ModelDomain instantiated es
    , sourceExecuteDomain :: DomainState 'ExecuteDomain instantiated es
    , sourceUpdateDomain :: DomainState 'UpdateDomain instantiated es
    , sourceDataflowArrows :: DataflowArrowCollection es
    , sourceGeometricTheory :: GeometricTheory
    , sourceMetadata :: TripletMetadata
    } -> MEUTripletEff instantiated validated executable es

  -- Branch triplet managing subtriplets
  BranchTripletEff ::
    { branchTripletId :: TripletId
    , branchParentId :: TripletId
    , branchChildren :: Set TripletId
    , branchModelDomain :: DomainState 'ModelDomain instantiated es
    , branchExecuteDomain :: DomainState 'ExecuteDomain instantiated es
    , branchUpdateDomain :: DomainState 'UpdateDomain instantiated es
    , branchDataflowArrows :: DataflowArrowCollection es
    , branchInclusionOps :: (InclusionOp 'ModelDomain es, InclusionOp 'ExecuteDomain es, InclusionOp 'UpdateDomain es)
    , branchRefinementFamily :: Set TripletId
    , branchGeometricTheory :: GeometricTheory
    , branchMetadata :: TripletMetadata
    } -> MEUTripletEff instantiated validated executable es

  -- Leaf triplet operating in execution environment
  LeafTripletEff ::
    { leafTripletId :: TripletId
    , leafParentId :: TripletId
    , leafAncestors :: [TripletId] -- Bottom-up inheritance chain
    , leafModelDomain :: DomainState 'ModelDomain Instantiated es
    , leafExecuteDomain :: DomainState 'ExecuteDomain Executable es
    , leafUpdateDomain :: DomainState 'UpdateDomain Validated es
    , leafDataflowArrows :: DataflowArrowCollection es
    , leafSUDPointers :: Map Text EnhancedSUDPointer -- Pointers to system under development
    , leafExecutionResults :: Map Text TypedValue
    , leafGeometricTheory :: GeometricTheory
    , leafMetadata :: TripletMetadata
    } -> MEUTripletEff Instantiated Validated Executable es

-- | Original MEU triplet (kept for backward compatibility)
data MEUTriplet = MEUTriplet
  { -- Core triplet identification
    tripletId :: TripletId
  , tripletType :: TripletType
  , tripletStatus :: TripletStatus

    -- Three interconnected domains
  , modelDomain :: Domain      -- M_τ: Model states and specifications
  , executeDomain :: Domain    -- E_τ: Execution environment
  , updateDomain :: Domain     -- U_τ: Evaluators and verifiers

    -- Dataflow arrows between domains
  , dataflowArrows :: Map ArrowType DataflowArrow

    -- MEU system integration
  , tripletParent :: Maybe TripletId
  , tripletChildren :: Set TripletId
  , tripletSiblings :: Set TripletId
  , tripletAncestors :: [TripletId] -- Bottom-up inheritance chain
  , refinementFamily :: Set TripletId -- All triplets in same refinement

    -- Inclusion relationships (covering sieves)
  , inclusionRelations :: Map TripletId InclusionType

    -- Source control integration
  , sourceControlBranch :: Maybe Text

    -- Acceptance criteria and geometric theory
  , acceptanceCriteria :: [AcceptanceCriteria]
  , geometricTheory :: GeometricTheory

    -- Metadata and timestamps
  , tripletMetadata :: TripletMetadata
  , tripletLastUpdated :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Inclusion relationship types for MEU triplet inheritance
data InclusionType
  = ParentInclusion     -- Child included in parent
  | ChildInclusion      -- Parent includes child
  | SiblingInclusion    -- Horizontal dependency
  | AncestorInclusion   -- Transitive inclusion
  deriving (Eq, Show, Ord, Generic)

-- | Geometric theory for acceptance criteria verification
data GeometricTheory = GeometricTheory
  { theoryVocabulary :: Set Text -- Signature Σ
  , theorySorts :: Set MEUType   -- Types E_i
  , theoryFunctionSymbols :: Set DSLPrimitiveId -- Function symbols
  , theoryRelationSymbols :: Set Text -- Relation symbols
  , theoryAxioms :: Set GeometricFormula -- Axioms as sequents
  , theoryConsistency :: Bool -- SMT-verified consistency
  } deriving (Eq, Show, Generic)