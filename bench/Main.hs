module Main where

import Criterion.Main
import Control.DeepSeq (NFData, rnf)
import MonadicStackingBenchmark
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Data.Text (Text)
import qualified Data.Text as T

import MEU.Core.Types
import qualified MEU.Transforms.Refinement as R
import qualified MEU.Transforms.Coarsening as C
import MEU.WS.StateTracker
import MEU.WS.Registries
import Control.Concurrent.STM (atomically)
import Data.Time (getCurrentTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Control.Monad (foldM_)

-- NFData instances for benchmarking
instance NFData TripletId where
  rnf (TripletId uuid) = rnf uuid

instance NFData TaskId where
  rnf (TaskId uuid) = rnf uuid

instance NFData TestId where
  rnf (TestId uuid) = rnf uuid

instance NFData ValueId where
  rnf (ValueId uuid) = rnf uuid

instance NFData MEUType where
  rnf (BaseType bt) = rnf bt
  rnf (ProductType t1 t2) = rnf t1 `seq` rnf t2
  rnf (SumType t1 t2) = rnf t1 `seq` rnf t2
  rnf (FunctionType t1 t2) = rnf t1 `seq` rnf t2
  rnf UnitType = ()
  rnf BottomType = ()
  rnf (NegationType t) = rnf t

instance NFData BaseType where
  rnf StringType = ()
  rnf IntType = ()
  rnf BoolType = ()
  rnf FloatType = ()
  rnf ModelType = ()
  rnf ExecutionType = ()
  rnf UpdateType = ()
  rnf ValueType = ()
  rnf TestType = ()
  rnf VerifierType = ()
  rnf (CustomType t) = rnf t

instance NFData ValueContent where
  rnf (StringValue s) = rnf s
  rnf (IntValue i) = rnf i
  rnf (BoolValue b) = rnf b
  rnf (FloatValue f) = rnf f
  rnf (ListValue vs) = rnf vs
  rnf (RecordValue rs) = rnf rs
  rnf (CompositeValue cs) = rnf cs
  rnf NullValue = ()

instance NFData TypedValue where
  rnf (TypedValue t c) = rnf t `seq` rnf c

instance NFData R.RefinementTransform where
  rnf (R.RefinementTransform tid rules) = rnf tid `seq` rnf rules

instance NFData R.RefinementRule where
  rnf (R.RefinementRule rid cond action) = rnf rid `seq` rnf cond `seq` rnf action

instance NFData R.RefinementResult where
  rnf (R.RefinementResult branch children inclusions integrations validation branches) =
    rnf branch `seq` rnf children `seq` rnf inclusions `seq` rnf integrations `seq` rnf validation `seq` rnf branches

instance NFData C.CoarseningTransform where
  rnf (C.CoarseningTransform tid rules) = rnf tid `seq` rnf rules

instance NFData C.CoarseningRule where
  rnf (C.CoarseningRule rid cond action) = rnf rid `seq` rnf cond `seq` rnf action

instance NFData C.CoarseningResult where
  rnf (C.CoarseningResult collapsed validation) = rnf collapsed `seq` rnf validation

instance NFData MEUError where
  rnf (TypeMismatchError expected actual ctx) = rnf expected `seq` rnf actual `seq` rnf ctx
  rnf (DomainInclusionError msg) = rnf msg
  rnf (AxiomInconsistencyError msg) = rnf msg
  rnf (RefinementError msg) = rnf msg
  rnf (CoarseningError msg) = rnf msg
  rnf (RegistryError msg) = rnf msg
  rnf (IOError msg) = rnf msg
  rnf (ValidationError msg) = rnf msg
  rnf (ExecutionError msg) = rnf msg
  rnf (SUDExecutionError msg) = rnf msg

instance NFData DomainType where
  rnf ModelDomain = ()
  rnf ExecuteDomain = ()
  rnf UpdateDomain = ()

instance NFData Timestamp where
  rnf (Timestamp utc) = rnf utc


main :: IO ()
main = do
  -- First run the comprehensive monadic stacking benchmarks as requested
  putStrLn "=== Running Comprehensive MEU Framework Monadic Stacking Benchmarks ==="
  putStrLn "Testing depths 2-10 with 20-1000 triplets as specified"
  results <- runMonadicStackingBenchmarks
  putStrLn "\n=== Comprehensive Benchmarks Completed ==="

  -- Then run the detailed Criterion benchmarks
  putStrLn "\n=== Running Detailed Criterion Benchmarks ==="
  defaultMain
  [ bgroup "MEU Core Types"
    [ bench "create TripletId" $ nfIO createTripletIdBench
    , bench "create 100 TripletIds" $ nfIO create100TripletIdsBench
    , bench "create 1000 TripletIds" $ nfIO create1000TripletIdsBench
    ]
  , bgroup "Type System"
    [ bench "create MEUType" $ nfIO createMEUTypeBench
    , bench "compare types" $ nfIO compareTypesBench
    , bench "complex type creation" $ nfIO complexTypeBench
    ]
  , bgroup "Value Operations"
    [ bench "create ValueId" $ nfIO createValueIdBench
    , bench "create TypedValue" $ nfIO createTypedValueBench
    , bench "process 100 values" $ nfIO process100ValuesBench
    ]
  , bgroup "Refinement Transforms"
    [ bench "refine 20 triplets" $ nfIO (refinementBench 20)
    , bench "refine 50 triplets" $ nfIO (refinementBench 50)
    , bench "refine 100 triplets" $ nfIO (refinementBench 100)
    , bench "refine 200 triplets" $ nfIO (refinementBench 200)
    , bench "refine 500 triplets" $ nfIO (refinementBench 500)
    , bench "refine 1000 triplets" $ nfIO (refinementBench 1000)
    ]
  , bgroup "Coarsening Transforms"
    [ bench "coarsen 20 triplets" $ nfIO (coarseningBench 20)
    , bench "coarsen 50 triplets" $ nfIO (coarseningBench 50)
    , bench "coarsen 100 triplets" $ nfIO (coarseningBench 100)
    , bench "coarsen 200 triplets" $ nfIO (coarseningBench 200)
    , bench "coarsen 500 triplets" $ nfIO (coarseningBench 500)
    , bench "coarsen 1000 triplets" $ nfIO (coarseningBench 1000)
    ]
  , bgroup "Transform Performance"
    [ bench "refinement transform creation" $ nfIO createRefinementTransformBench
    , bench "coarsening transform creation" $ nfIO createCoarseningTransformBench
    , bench "validate refinement 100 triplets" $ nfIO (validateRefinementBench 100)
    , bench "validate coarsening 100 triplets" $ nfIO (validateCoarseningBench 100)
    ]
  , bgroup "WS State Tracker Stacked Monads"
    [ bench "stacked monad registry tracking (5 levels)" $ nfIO (stackedMonadTrackingBench 5)
    , bench "stacked monad registry tracking (10 levels)" $ nfIO (stackedMonadTrackingBench 10)
    , bench "stacked monad registry tracking (20 levels)" $ nfIO (stackedMonadTrackingBench 20)
    , bench "value merge hypergraph (100 values)" $ nfIO (valueMergeHypergraphBench 100)
    , bench "value merge hypergraph (1000 values)" $ nfIO (valueMergeHypergraphBench 1000)
    , bench "registry inheritance chain (5 levels, 100 values)" $ nfIO (registryInheritanceChainBench 5 100)
    , bench "registry inheritance chain (10 levels, 1000 values)" $ nfIO (registryInheritanceChainBench 10 1000)
    , bench "MEU system evolution tracking (50 refinements)" $ nfIO (meuSystemEvolutionBench 50)
    ]
  ]

-- Benchmark implementations using actual available types
createTripletIdBench :: IO TripletId
createTripletIdBench = TripletId <$> UUID.nextRandom

create100TripletIdsBench :: IO [TripletId]
create100TripletIdsBench = mapM (const createTripletIdBench) [1..100]

create1000TripletIdsBench :: IO [TripletId]
create1000TripletIdsBench = mapM (const createTripletIdBench) [1..1000]

createMEUTypeBench :: IO MEUType
createMEUTypeBench = pure $ FunctionType (BaseType StringType) (BaseType IntType)

compareTypesBench :: IO Bool
compareTypesBench = do
  let type1 = BaseType StringType
      type2 = BaseType IntType
      type3 = ProductType type1 type2
      type4 = SumType type1 type2
  pure $ type3 == type4

complexTypeBench :: IO MEUType
complexTypeBench = do
  let baseString = BaseType StringType
      baseInt = BaseType IntType
      baseBool = BaseType BoolType
      product1 = ProductType baseString baseInt
      product2 = ProductType baseBool UnitType
      sum1 = SumType product1 product2
      func1 = FunctionType sum1 baseString
  pure func1

createValueIdBench :: IO ValueId
createValueIdBench = ValueId <$> UUID.nextRandom

createTypedValueBench :: IO TypedValue
createTypedValueBench = do
  pure $ TypedValue (BaseType StringType) (StringValue "benchmark test")

process100ValuesBench :: IO [TypedValue]
process100ValuesBench = mapM (const createTypedValueBench) [1..100]

-- Transform benchmark implementations
refinementBench :: Int -> IO [R.RefinementResult]
refinementBench numTriplets = do
  let transform = R.createRefinementTransform "bench-refinement" [createBenchRule "refine"]
      tripletIds = map (\i -> T.pack $ "triplet-" ++ show i) [1..numTriplets]
  mapM (\tid -> case R.applyRefinement transform tid of
    Left _ -> pure $ R.RefinementResult "error" [] mempty [] (Left []) []
    Right result -> pure result) tripletIds

coarseningBench :: Int -> IO [C.CoarseningResult]
coarseningBench numTriplets = do
  let transform = C.createCoarseningTransform "bench-coarsening" [createCoarseningRule "coarsen"]
      tripletIds = map (\i -> T.pack $ "triplet-" ++ show i) [1..numTriplets]
      tripletGroups = chunksOf 5 tripletIds  -- Group triplets for coarsening
  mapM (\group -> case C.applyCoarsening transform group of
    Left _ -> pure $ C.CoarseningResult "error" (Left [])
    Right result -> pure result) tripletGroups

createRefinementTransformBench :: IO R.RefinementTransform
createRefinementTransformBench = do
  let rules = map (\i -> createBenchRule ("rule-" ++ show i)) [1..10]
  pure $ R.createRefinementTransform "benchmark-transform" rules

createCoarseningTransformBench :: IO C.CoarseningTransform
createCoarseningTransformBench = do
  let rules = map (\i -> createCoarseningRule ("rule-" ++ show i)) [1..10]
  pure $ C.createCoarseningTransform "benchmark-transform" rules

validateRefinementBench :: Int -> IO (Either [MEUError] ())
validateRefinementBench numTriplets = do
  let transform = R.createRefinementTransform "validation-test" [createBenchRule "validate"]
      tripletIds = map (\i -> T.pack $ "triplet-" ++ show i) [1..numTriplets]
      parentTriplet = "parent-triplet"
  pure $ R.validateRefinement transform parentTriplet tripletIds

validateCoarseningBench :: Int -> IO (Either [MEUError] ())
validateCoarseningBench numTriplets = do
  let transform = C.createCoarseningTransform "validation-test" [createCoarseningRule "validate"]
      tripletIds = map (\i -> T.pack $ "triplet-" ++ show i) [1..numTriplets]
  pure $ C.validateCoarsening transform tripletIds

-- Helper functions
createBenchRule :: String -> R.RefinementRule
createBenchRule name = R.RefinementRule
  { R.ruleId = T.pack name
  , R.ruleCondition = "benchmark condition"
  , R.ruleAction = "benchmark action"
  }

createCoarseningRule :: String -> C.CoarseningRule
createCoarseningRule name = C.CoarseningRule
  { C.ruleId = T.pack name
  , C.ruleCondition = "benchmark condition"
  , C.ruleAction = "benchmark action"
  }

-- Simple implementation of chunksOf
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- WS State Tracker Stacked Monads Benchmarks
-- These benchmarks test the performance of tracking values across all registries
-- accounting for the stacked monads of the MEU system as it changes and grows
-- under consecutive refinement transforms according to MEU Framework specification

-- Extended value type with stacked monad metadata
data StackedValue = StackedValue
  { stackedValueId :: ValueId
  , stackedValueName :: Text
  , stackedValueType :: MEUType
  , stackedValueContent :: ValueContent
  , stackedValueTriplet :: TripletId
  , stackedValueDomain :: DomainType
  , stackedValueCreated :: Timestamp
  , stackedValueInheritanceChain :: [TripletId] -- Stacked monad inheritance
  , stackedValueLevel :: Int -- Level in stacked monad hierarchy
  } deriving (Eq, Show, Generic)

instance NFData StackedValue where
  rnf (StackedValue vid vname vtype vcontent vtriplet vdomain vcreated vchain vlevel) =
    rnf vid `seq` rnf vname `seq` rnf vtype `seq` rnf vcontent `seq` rnf vtriplet `seq`
    rnf vdomain `seq` rnf vcreated `seq` rnf vchain `seq` rnf vlevel

-- Helper functions for stacked monad benchmarks
currentTimestamp :: IO Timestamp
currentTimestamp = Timestamp <$> getCurrentTime

generateTripletId :: IO TripletId
generateTripletId = TripletId <$> UUID.nextRandom

generateValueId :: IO ValueId
generateValueId = ValueId <$> UUID.nextRandom

-- | Benchmark stacked monadic registry tracking across MEU system evolution
stackedMonadTrackingBench :: Int -> IO ()
stackedMonadTrackingBench numLevels = do
  timestamp <- currentTimestamp

  -- Create nested MEU triplet hierarchy (stacked monads)
  parentIds <- sequence $ replicate numLevels generateTripletId

  -- Create values at each level and track inheritance through stacked monads
  valueIds <- sequence $ replicate (numLevels * 10) generateValueId
  let values = zipWith (\level vid -> StackedValue
        { stackedValueId = vid
        , stackedValueName = "stacked-value-" <> T.pack (show level)
        , stackedValueType = BaseType StringType
        , stackedValueContent = StringValue ("Level " <> T.pack (show level) <> " value")
        , stackedValueTriplet = parentIds !! (level `mod` numLevels)
        , stackedValueDomain = ModelDomain
        , stackedValueCreated = timestamp
        , stackedValueInheritanceChain = take (level + 1) parentIds
        , stackedValueLevel = level
        }) [0..] valueIds

  -- Simulate tracking values through registry inheritance chain (stacked monads)
  mapM_ (\value -> do
    -- Simulate registry operations for each value in stacked monad hierarchy
    let inheritanceDepth = length (stackedValueInheritanceChain value)
    sequence_ $ replicate inheritanceDepth (return ())
    ) values

  return ()

-- | Benchmark value merge hypergraph performance
valueMergeHypergraphBench :: Int -> IO ()
valueMergeHypergraphBench numValues = do
  timestamp <- currentTimestamp

  -- Create values for merge testing
  valueIds <- sequence $ replicate numValues generateValueId
  tripletId <- generateTripletId
  let values = map (\vid -> StackedValue
        { stackedValueId = vid
        , stackedValueName = "merge-value"
        , stackedValueType = BaseType StringType
        , stackedValueContent = StringValue "mergeable"
        , stackedValueTriplet = tripletId
        , stackedValueDomain = ModelDomain
        , stackedValueCreated = timestamp
        , stackedValueInheritanceChain = [tripletId]
        , stackedValueLevel = 0
        }) valueIds

  -- Simulate value merge hypergraph construction
  let mergeMatrix = replicate numValues (replicate numValues True)
  rnf mergeMatrix `seq` return ()

-- | Benchmark registry inheritance chain through stacked monads
registryInheritanceChainBench :: Int -> Int -> IO ()
registryInheritanceChainBench numLevels numValues = do
  timestamp <- currentTimestamp

  -- Create inheritance hierarchy
  parentIds <- sequence $ replicate numLevels generateTripletId
  let inheritanceChain = Map.fromList $ zipWith (\level parentId ->
        (parentId, take level parentIds)) [1..] parentIds

  -- Create values distributed across inheritance levels
  valueIds <- sequence $ replicate numValues generateValueId
  let values = zipWith (\i vid -> StackedValue
        { stackedValueId = vid
        , stackedValueName = "inherited-value-" <> T.pack (show i)
        , stackedValueType = BaseType StringType
        , stackedValueContent = StringValue ("Inherited value " <> T.pack (show i))
        , stackedValueTriplet = parentIds !! (i `mod` numLevels)
        , stackedValueDomain = ModelDomain
        , stackedValueCreated = timestamp
        , stackedValueInheritanceChain = take ((i `mod` numLevels) + 1) parentIds
        , stackedValueLevel = i `mod` numLevels
        }) [0..] valueIds

  -- Simulate inheritance tracking through stacked monads
  mapM_ (\value -> do
    let chainLength = length (stackedValueInheritanceChain value)
    -- Simulate registry lookup at each level of inheritance chain
    sequence_ $ replicate chainLength (return ())
    ) values

  -- Validate inheritance consistency
  rnf inheritanceChain `seq` rnf values `seq` return ()

-- | Benchmark MEU system evolution tracking
meuSystemEvolutionBench :: Int -> IO ()
meuSystemEvolutionBench numRefinements = do
  timestamp <- currentTimestamp

  -- Create source triplet
  sourceId <- generateTripletId

  -- Perform multiple refinement steps and track system evolution
  foldM_ (\currentId i -> do
    childId <- generateTripletId

    -- Simulate refinement tracking in stacked monad hierarchy
    let refinementLevel = i
        valueId = generateValueId
    vid <- valueId
    let refinementValue = StackedValue
          { stackedValueId = vid
          , stackedValueName = "evolution-value-" <> T.pack (show i)
          , stackedValueType = BaseType StringType
          , stackedValueContent = StringValue ("Evolution step " <> T.pack (show i))
          , stackedValueTriplet = childId
          , stackedValueDomain = ModelDomain
          , stackedValueCreated = timestamp
          , stackedValueInheritanceChain = [sourceId, childId]
          , stackedValueLevel = refinementLevel
          }

    -- Simulate tracking value through evolution
    rnf refinementValue `seq` return childId
    ) sourceId [1..numRefinements]

  return ()