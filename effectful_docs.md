Effectful

Contents

Introduction
Integration with existing libraries
Transformed monads
Concrete monads
IO
Other
Polymorphic monads
The Eff monad
Effect constraints
Running the Eff monad
Pure computations
Computations with side effects
Unlifting
Lifting
Re-exports
Synopsis
Introduction
Haskell is one of the few programming languages that distinguishes between pure functions and functions that might perform side effects. For example, a function

f :: Int -> String
can't perform side effects at all, but a function

f :: Int -> IO String
can perform any side effect. This "all or nothing" approach isn't very satisfactory though, because the vast majority of time we would like to signify that a function can perform some side effects, e.g. only be able to log messages.

This library provides support for expressing exactly that with its Eff monad:

f :: Log :> es => Int -> Eff es String
It implements support for extensible effects with both dynamic and static dispatch. For more information about each type consult the documentation in Effectful.Dispatch.Dynamic and Effectful.Dispatch.Static (when in doubt, start with dynamic dispatch).

The library provides:

The Eff monad that tracks effects at the type level. This is going to be the main monad of your application.
A set of predefined, basic effects such as Error, Reader, State and Writer.
Utilities for defining new effects and interpreting them, possibly in terms of already existing ones (see introduction of the Effectful.Dispatch.Dynamic module for more information).
While basic effects can be used out of the box, in general it's recommended to create your own that serve a more specific purpose.

Integration with existing libraries
Integration with most of existing libraries and frameworks can be done quite easily. The main difference in how that looks like depends on the way a library operates in a monadic context.

There are three main groups a library might fall into. It either operates:

1) In a monad of your application transformed by a library specific monad transformer.

2) In its own, concrete monad, which is usually IO or a couple of monad transformers on top of IO.

3) In a polymorphic monad, which is constrained by a type class that implements core operations of a library.

Each case needs a slightly different approach to integrate with the Eff monad.

Transformed monads
These are libraries that provide a custom transformer for the main monad of your application and their operations make use of it for their operations. Examples include InputT from the haskeline package or ConduitT from the conduit package.

These libraries can trivially be used with the Eff monad since it provides typical instances that these libraries require the underlying monad to have, such as MonadMask or MonadUnliftIO.

In case the Eff monad doesn't provide a specific instance out of the box, it can be supplied via an effect. As an example see how the instance of MonadResource for Eff is implemented in the resourcet-effectful package.

Concrete monads
IO
If a library operates in IO, there are a couple of ways to integrate it.

The easiest way is to use its functions selectively in the Eff monad with the help of liftIO or withEffToIO. However, this is not particularly robust, since it vastly broadens the scope in which the IOE effect is needed (not to mention that explicit lifting is annoying).

A somewhat better approach is to create a dummy static effect with lightweight wrappers of the library functions. As an example have a look at the Effectful.Concurrent.Async module from the effectful package that wraps the API of the async package. Unfortunately, this requires the amount of work proportional to the size of the library and might not be the best option, especially if you only need to make use of a tiny portion of the API.

Even better (though sometimes hard to do in practice) way is to consider, what do you need the library for and then create a custom effect with high level operations that the library in question will help us implement. The advantage of this approach is that we're hiding implementation details from the so-called "business logic" of our application and make it possible to easily swap them in different environments or during future refactoring.

Other
Some libraries operate in a transformer stack over IO or have its own concrete monad that's a newtype over IO, e.g. Handler from the servant-server package.

In such case it's best to mirror the monad in question by the Eff monad with appropriate effects (as most popular monad transformers have subtle issues), use it as soon as possible, then at the end feed the final state to the monad of the library so it proceeds as if nothing unusual happened.

As an example, consider the following monad:

>>> import Control.Monad.State qualified as T
>>> import Control.Monad.Except qualified as T
>>> data HandlerState
>>> data HandlerError
>>> :{
  newtype Handler a = Handler (T.ExceptT HandlerError (T.StateT HandlerState IO) a)
    deriving ( Applicative, Functor, Monad, MonadIO
             , T.MonadState HandlerState, T.MonadError HandlerError
             )
:}
This is how you can execute Eff actions in the Handler monad:

>>> import Effectful.Error.Static
>>> import Effectful.State.Static.Local
>>> :{
  effToHandler :: Eff [Error HandlerError, State HandlerState, IOE] a -> Handler a
  effToHandler m = do
    -- Retrieve the current state of the Handler.
    s <- T.get
    -- Run the Eff monad with effects mirroring the capabilities of @Handler@.
    (er, s') <- liftIO . runEff . runState s . runErrorNoCallStack @HandlerError $ m
    -- Update the state of the Handler and throw an error if appropriate.
    T.put s'
    either T.throwError pure er
:}
Polymorphic monads
Libraries working in a polymorphic monad use mtl style effects. Details about their integration with the Eff monad require familiarity with dynamically dispatched effects and thus are available in the Effectful.Dispatch.Dynamic module.

The Eff monad
data Eff (es :: [Effect]) aSource#

The Eff monad provides the implementation of a computation that performs an arbitrary set of effects. In Eff es a, es is a type-level list that contains all the effects that the computation may perform. For example, a computation that produces an Integer by consuming a String from the global environment and acting upon a single mutable value of type Bool would have the following type:

(Reader String :> es, State Bool :> es) => Eff es Integer
Abstracting over the list of effects with (:>):

Allows the computation to be used in functions that may perform other effects.
Allows the effects to be handled in any order.
Instances
 IOE :> es => MonadBaseControl IO (Eff es)Source#	
Instance included for compatibility with existing code.

Usage of withEffToIO is preferrable as it allows specifying the UnliftStrategy on a case-by-case basis and has better error reporting.

Note: the unlifting strategy for liftBaseWith is taken from the IOE context (see unliftStrategy).

 (Show e, Error e :> es, MonadError e (Eff es)) => MonadError e (Eff es)Source#	
Instance included for compatibility with existing code.

 (Reader r :> es, MonadReader r (Eff es)) => MonadReader r (Eff es)Source#	
Instance included for compatibility with existing code.

 (State s :> es, MonadState s (Eff es)) => MonadState s (Eff es)Source#	
Instance included for compatibility with existing code.

 (Monoid w, Writer w :> es, MonadWriter w (Eff es)) => MonadWriter w (Eff es)Source#	
Instance included for compatibility with existing code.

 IOE :> es => MonadBase IO (Eff es)Source#	
Instance included for compatibility with existing code.

Usage of liftIO is preferrable as it's a standard.

 Fail :> es => MonadFail (Eff es)Source#	 
 MonadFix (Eff es)Source#	 
 IOE :> es => MonadIO (Eff es)Source#	 
 NonDet :> es => Alternative (Eff es)Source#	
Since: 2.2.0.0

 Applicative (Eff es)Source#	 
 Functor (Eff es)Source#	 
 Monad (Eff es)Source#	 
 NonDet :> es => MonadPlus (Eff es)Source#	
Since: 2.2.0.0

 MonadCatch (Eff es)Source#	 
 MonadMask (Eff es)Source#	 
 MonadThrow (Eff es)Source#	 
 Prim :> es => PrimMonad (Eff es)Source#	 
 IOE :> es => MonadUnliftIO (Eff es)Source#	
Instance included for compatibility with existing code.

Usage of withEffToIO is preferrable as it allows specifying the UnliftStrategy on a case-by-case basis and has better error reporting.

Note: the unlifting strategy for withRunInIO is taken from the IOE context (see unliftStrategy).

 Monoid a => Monoid (Eff es a)Source#	 
 Semigroup a => Semigroup (Eff es a)Source#	 
 type PrimState (Eff es)Source#	 
 type StM (Eff es) aSource#	 
Effect constraints
type Effect = (Type -> Type) -> Type -> TypeSource#

The kind of effects.

data DispatchSource#

A type of dispatch. For more information consult the documentation in Effectful.Dispatch.Dynamic and Effectful.Dispatch.Static.

Constructors

Dynamic	 
Static SideEffects	 
type family DispatchOf (e :: Effect) :: DispatchSource#

Dispatch types of effects.

Instances
 type DispatchOf FailSource#	 
 type DispatchOf IOESource#	 
 type DispatchOf NonDetSource#	 
 type DispatchOf PrimSource#	 
 type DispatchOf (Error e)Source#	 
 type DispatchOf (Error e)Source#	 
 type DispatchOf (Reader r)Source#	 
 type DispatchOf (State s)Source#	 
 type DispatchOf (Writer w)Source#	 
 type DispatchOf (Reader r)Source#	 
 type DispatchOf (State s)Source#	 
 type DispatchOf (State s)Source#	 
 type DispatchOf (Writer w)Source#	 
 type DispatchOf (Writer w)Source#	 
 type DispatchOf (Labeled label e)Source#	 
 type DispatchOf (Provider e input f)Source#	 
 type DispatchOf (ProviderList providedEs input f)Source#	 
class (e :: Effect) :> (es :: [Effect])Source#

A constraint that requires that a particular effect e is a member of the type-level list es. This is used to parameterize an Eff computation over an arbitrary list of effects, so long as e is somewhere in the list.

For example, a computation that only needs access to a mutable value of type Integer would have the following type:

State Integer :> es => Eff es ()
Instances
 (TypeError (('Text "There is no handler for '" ':<>: 'ShowType e) ':<>: 'Text "' in the context") :: Constraint) => e :> ('[] :: [Effect])Source#	 
 e :> (e ': es)Source#	 
 e :> es => e :> (x ': es)Source#	 
type family (xs :: [Effect]) :>> (es :: [Effect]) where ...Source#

Deprecated: Usage of (:>>) slows down GHC too much. See https://github.com/haskell-effectful/effectful/issues/52#issuecomment-1269155485 for more information.

Convenience operator for expressing that a function uses multiple effects in a more concise way than enumerating them all with (:>).

[E1, E2, ..., En] :>> es ≡ (E1 :> es, E2 :> es, ..., En :> es)
Equations

('[] :: [Effect]) :>> es = ()	 
(x ': xs) :>> es = (x :> es, xs :>> es)	 
Running the Eff monad
Pure computations
runPureEff :: HasCallStack => Eff ('[] :: [Effect]) a -> aSource#

Run a pure Eff computation.

For running computations with side effects see runEff.

Computations with side effects
runEff :: HasCallStack => Eff '[IOE] a -> IO aSource#

Run an Eff computation with side effects.

For running pure computations see runPureEff.

data IOE (a :: Type -> Type) bSource#

Run arbitrary IO computations via MonadIO or MonadUnliftIO.

Note: it is not recommended to use this effect in application code as it is too liberal. Ideally, this is only used in handlers of more fine-grained effects.

Instances
 type DispatchOf IOESource#	 
 newtype StaticRep IOESource#	 
Unlifting
data UnliftStrategySource#

The strategy to use when unlifting Eff computations via withEffToIO or the localUnlift family.

Constructors

SeqUnlift	
The sequential strategy is the fastest and a default setting for IOE. Any attempt of calling the unlifting function in threads distinct from its creator will result in a runtime error.

SeqForkUnlift	
Like SeqUnlift, but all unlifted actions will be executed in a cloned environment.

The main consequence is that thread local state is forked at the point of creation of the unlifting function and its modifications in unlifted actions will not affect the main thread of execution (and vice versa):

>>> import Effectful
>>> import Effectful.State.Dynamic
>>> :{
 action :: (IOE :> es, State Int :> es) => Eff es ()
 action = do
   modify @Int (+1)
   withEffToIO SeqForkUnlift $ \unlift -> unlift $ modify @Int (+2)
   modify @Int (+4)
:}
>>> runEff . execStateLocal @Int 0 $ action
5
>>> runEff . execStateShared @Int 0 $ action
7
Because of this it's possible to safely use the unlifting function outside of the scope of effects it captures, e.g. by creating an IO action that executes effectful operations and running it later:

>>> :{
  delayed :: UnliftStrategy -> IO (IO String)
  delayed strategy = runEff . evalStateLocal "Hey" $ do
    r <- withEffToIO strategy $ \unlift -> pure $ unlift get
    modify (++ "!!!")
    pure r
:}
This doesn't work with the SeqUnlift strategy because when the returned action runs, State is no longer in scope:

>>> join $ delayed SeqUnlift
*** Exception: version (...) /= storageVersion (0)
...
However, it does with the SeqForkUnlift strategy:

>>> join $ delayed SeqForkUnlift
"Hey"
ConcUnlift !Persistence !Limit	
The concurrent strategy makes it possible for the unlifting function to be called in threads distinct from its creator. See Persistence and Limit settings for more information.

Instances
 Generic UnliftStrategySource#	 
 Show UnliftStrategySource#	 
 Eq UnliftStrategySource#	 
 Ord UnliftStrategySource#	 
 type Rep UnliftStrategySource#	 
data PersistenceSource#

Persistence setting for the ConcUnlift strategy.

Different functions require different persistence strategies. Examples:

Lifting pooledMapConcurrentlyN from the unliftio library requires the Ephemeral strategy as we don't want jobs to share environment changes made by previous jobs run in the same worker thread.
Lifting forkIOWithUnmask requires the Persistent strategy, otherwise the unmasking function would start with a fresh environment each time it's called.
Constructors

Ephemeral	
Don't persist the environment between calls to the unlifting function in threads distinct from its creator.

Persistent	
Persist the environment between calls to the unlifting function within a particular thread.

Instances
 Generic PersistenceSource#	 
 Show PersistenceSource#	 
 Eq PersistenceSource#	 
 Ord PersistenceSource#	 
 type Rep PersistenceSource#	 
data LimitSource#

Limit setting for the ConcUnlift strategy.

Constructors

Limited !Int	
Behavior dependent on the Persistence setting.

For Ephemeral, it limits the amount of uses of the unlifting function in threads distinct from its creator to N. The unlifting function will create N copies of the environment when called N times and K+1 copies when called K < N times.

For Persistent, it limits the amount of threads, distinct from the creator of the unlifting function, it can be called in to N. The amount of calls to the unlifting function within a particular threads is unlimited. The unlifting function will create N copies of the environment when called in N threads and K+1 copies when called in K < N threads.

Unlimited	
Unlimited use of the unlifting function.

Instances
 Generic LimitSource#	 
 Show LimitSource#	 
 Eq LimitSource#	 
 Ord LimitSource#	 
 type Rep LimitSource#	 
unliftStrategy :: forall (es :: [Effect]). IOE :> es => Eff es UnliftStrategySource#

Get the current UnliftStrategy.

Note: this strategy is implicitly used by the MonadUnliftIO and MonadBaseControl instance for Eff.

withUnliftStrategy :: forall (es :: [Effect]) a. IOE :> es => UnliftStrategy -> Eff es a -> Eff es aSource#

Locally override the current UnliftStrategy with the given value.

withSeqEffToIOSource#

:: forall (es :: [Effect]) a. (HasCallStack, IOE :> es)	 
=> ((forall r. Eff es r -> IO r) -> IO a)	
Continuation with the unlifting function in scope.

-> Eff es a	 
Create an unlifting function with the SeqUnlift strategy. For the general version see withEffToIO.

Note: usage of this function is preferrable to withRunInIO because of explicit unlifting strategy and better error reporting.

Since: 2.2.2.0

withEffToIOSource#

:: forall (es :: [Effect]) a. (HasCallStack, IOE :> es)	 
=> UnliftStrategy	 
-> ((forall r. Eff es r -> IO r) -> IO a)	
Continuation with the unlifting function in scope.

-> Eff es a	 
Create an unlifting function with the given strategy.

Note: usage of this function is preferrable to withRunInIO because of explicit unlifting strategy and better error reporting.

Lifting
raise :: forall (e :: Effect) (es :: [Effect]) a. Eff es a -> Eff (e ': es) aSource#

Lift an Eff computation into an effect stack with one more effect.

raiseWithSource#

:: forall (e :: Effect) (es :: [Effect]) a. HasCallStack	 
=> UnliftStrategy	 
-> ((forall r. Eff (e ': es) r -> Eff es r) -> Eff es a)	
Continuation with the unlifting function in scope.

-> Eff (e ': es) a	 
Lift an Eff computation into an effect stack with one more effect and create an unlifting function with the given strategy.

Since: 1.2.0.0

subsume :: forall (e :: Effect) (es :: [Effect]) a. e :> es => Eff (e ': es) a -> Eff es aSource#

Eliminate a duplicate effect from the top of the effect stack.

inject :: forall (subEs :: [Effect]) (es :: [Effect]) a. Subset subEs es => Eff subEs a -> Eff es aSource#

Allow for running an effect stack subEs within es as long as subEs is a permutation (with possible duplicates) of a subset of es.

Generalizes raise and subsume.

>>> data E1 :: Effect
>>> data E2 :: Effect
>>> data E3 :: Effect
It makes it possible to rearrange the effect stack however you like:

>>> :{
  shuffle :: Eff (E3 : E1 : E2 : es) a -> Eff (E1 : E2 : E3 : es) a
  shuffle = inject
:}
It can also turn a monomorphic effect stack into a polymorphic one:

>>> :{
  toPoly :: (E1 :> es, E2 :> es, E3 :> es) => Eff [E1, E2, E3] a -> Eff es a
  toPoly = inject
:}
Moreover, it allows for hiding specific effects from downstream:

>>> :{
  onlyE1 :: Eff (E1 : es) a -> Eff (E1 : E2 : E3 : es) a
  onlyE1 = inject
:}
>>> :{
  onlyE2 :: Eff (E2 : es) a -> Eff (E1 : E2 : E3 : es) a
  onlyE2 = inject
:}
>>> :{
  onlyE3 :: Eff (E3 : es) a -> Eff (E1 : E2 : E3 : es) a
  onlyE3 = inject
:}
However, it's not possible to inject a computation into an incompatible effect stack:

>>> :{
  coerceEs :: Eff es1 a -> Eff es2 a
  coerceEs = inject
:}
...
...Couldn't match type ‘es1’ with ‘es2’
...
class KnownPrefix es => Subset (subEs :: [Effect]) (es :: [Effect])Source#

Provide evidence that subEs is a subset of es.

Instances
 (KnownPrefix es, IsUnknownSuffixOf subEs es) => Subset subEs esSource#	 
 KnownPrefix es => Subset ('[] :: [Effect]) esSource#	 
 (e :> es, Subset subEs es) => Subset (e ': subEs) esSource#	 
Re-exports
class Monad m => MonadIO (m :: Type -> Type) where#

Monads in which IO computations may be embedded. Any monad built by applying a sequence of monad transformers to the IO monad will be an instance of this class.

Instances should satisfy the following laws, which state that liftIO is a transformer of monads:

liftIO . return = return
liftIO (m >>= f) = liftIO m >>= (liftIO . f)
Methods

liftIO :: IO a -> m a#

Lift a computation from the IO monad. This allows us to run IO computations in any monadic stack, so long as it supports these kinds of operations (i.e. IO is the base monad for the stack).

Example
Instances
 MonadIO IO	
Since: base-4.9.0.0

 MonadIO Q	 
 IOE :> es => MonadIO (Eff es)Source#	 
 MonadIO m => MonadIO (CatchT m)	 
 MonadIO m => MonadIO (MaybeT m)	 
 (Monoid w, Functor m, MonadIO m) => MonadIO (AccumT w m)	 
 MonadIO m => MonadIO (ExceptT e m)	 
 MonadIO m => MonadIO (IdentityT m)	 
 MonadIO m => MonadIO (ReaderT r m)	 
 MonadIO m => MonadIO (SelectT r m)	 
 MonadIO m => MonadIO (StateT s m)	 
 MonadIO m => MonadIO (StateT s m)	 
 MonadIO m => MonadIO (WriterT w m)	 
 (Monoid w, MonadIO m) => MonadIO (WriterT w m)	 
 (Monoid w, MonadIO m) => MonadIO (WriterT w m)	 
 MonadIO m => MonadIO (ContT r m)	 
 MonadIO m => MonadIO (RWST r w s m)	 
 (Monoid w, MonadIO m) => MonadIO (RWST r w s m)	 
 (Monoid w, MonadIO m) => MonadIO (RWST r w s m)	 
class MonadIO m => MonadUnliftIO (m :: Type -> Type) where#

Monads which allow their actions to be run in IO.

While MonadIO allows an IO action to be lifted into another monad, this class captures the opposite concept: allowing you to capture the monadic context. Note that, in order to meet the laws given below, the intuition is that a monad must have no monadic state, but may have monadic context. This essentially limits MonadUnliftIO to ReaderT and IdentityT transformers on top of IO.

Laws. For any function run provided by withRunInIO, it must meet the monad transformer laws as reformulated for MonadUnliftIO:

run . return = return
run (m >>= f) = run m >>= run . f
Instances of MonadUnliftIO must also satisfy the following laws:

Identity law
withRunInIO (\run -> run m) = m
Inverse law
withRunInIO (\_ -> m) = liftIO m
As an example of an invalid instance, a naive implementation of MonadUnliftIO (StateT s m) might be

withRunInIO inner =
  StateT $ \s ->
    withRunInIO $ \run ->
      inner (run . flip evalStateT s)
This breaks the identity law because the inner run m would throw away any state changes in m.

Since: unliftio-core-0.1.0.0

Methods

withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b#

Convenience function for capturing the monadic context and running an IO action with a runner function. The runner function is used to run a monadic action m in IO.

Since: unliftio-core-0.1.0.0

Instances
 MonadUnliftIO IO	 
 IOE :> es => MonadUnliftIO (Eff es)Source#	
Instance included for compatibility with existing code.

Usage of withEffToIO is preferrable as it allows specifying the UnliftStrategy on a case-by-case basis and has better error reporting.

Note: the unlifting strategy for withRunInIO is taken from the IOE context (see unliftStrategy).

 MonadUnliftIO m => MonadUnliftIO (IdentityT m)	 
 MonadUnliftIO m => MonadUnliftIO (ReaderT r m)	 
Produced by Haddock version 2.30.0