{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module MEU.DSL.Types
  ( -- * Re-exported from Core.Types
    DSLPrimitiveId(..)
  , DSLPrimitive(..)
  , DSLFunction(..)
  , ValueId(..)
  , TypedValue(..)
  , ValueContent(..)
  , DataflowArrow(..)
  , ArrowType(..)

    -- * DSL-specific types
  , DataflowCollection(..)
  , RegistryEntry(..)
  , RegistryType(..)
  , ExecutionContext(..)
  , ExecutionResult(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

-- Re-export types from Core.Types
import MEU.Core.Types
  ( DSLPrimitiveId(..)
  , DSLPrimitive(..)
  , DSLFunction(..)
  , ValueId(..)
  , TypedValue(..)
  , ValueContent(..)
  , DataflowArrow(..)
  , ArrowType(..)
  , TypeSignature(..)
  , DomainId(..)
  , TripletId(..)
  , Timestamp(..)
  , MEUError(..)
  , TestId(..)
  )

-- | Collection of dataflow arrows for MEU triplet
data DataflowCollection = DataflowCollection
  { iArrows :: [DataflowArrow]      -- {I: M->E}
  , iStarArrows :: [DataflowArrow]  -- {I*: E->M}
  , oArrows :: [DataflowArrow]      -- {O: E->U}
  , oStarArrows :: [DataflowArrow]  -- {O*: U->E}
  , rArrows :: [DataflowArrow]      -- {R: U->M}
  , rStarArrows :: [DataflowArrow]  -- {R*: M->U}
  , identityArrows :: [DataflowArrow] -- Identity mappings
  , dependencyArrows :: [DataflowArrow] -- Horizontal dependencies
  } deriving (Eq, Show, Generic)

-- Registry entry for DSL operations
data RegistryEntry = RegistryEntry
  { registryEntryId :: Text
  , registryEntryType :: RegistryType
  , registryEntryValue :: Text -- Simplified for now
  , registryEntryMetadata :: Map Text Text
  , registryEntryCreated :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Registry type classification
data RegistryType
  = TypeRegistryEntry
  | ValueRegistryEntry
  | PrimitiveRegistryEntry
  | TestRegistryEntry
  | VerifierRegistryEntry
  | AxiomRegistryEntry
  | TripletRegistryEntry
  deriving (Eq, Show, Ord, Generic)

-- | Execution context for DSL functions
data ExecutionContext = ExecutionContext
  { contextTripletId :: TripletId
  , contextDomainId :: DomainId
  , contextEnvironment :: Map Text Text
  , contextTimestamp :: Timestamp
  } deriving (Eq, Show, Generic)

-- | Result of DSL function execution
data ExecutionResult = ExecutionResult
  { resultValue :: Either MEUError TypedValue
  , resultContext :: ExecutionContext
  , resultMetrics :: Map Text Text
  , resultDuration :: Double -- milliseconds
  } deriving (Eq, Show, Generic)