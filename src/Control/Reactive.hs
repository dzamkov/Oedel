{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Reactive where

import Data.Monoid
import Control.Monad.Identity
import Control.Applicative

-- TODO: remove once AMP goes through
type MonadFix' m = (Applicative m, MonadFix m)

-- | @e@ is a type constructor for an event in a reactive system. An event
-- can be thought of as a stream of (a countable number) of occurences.
-- There is a systemwide partial ordering of event occurences, which we use
-- to define /before/ and /after/. The 'Functor' instance of @e@ maps the
-- values of occurences of an event while preserving the order of occurences
-- with respect to all others.
class (Functor e, MonadFix' (Moment e)) => Event e where

    -- | A monad that carries a time-dependent value.
    type Moment e :: * -> *

    -- | Instantiates time-dependent values with the time they occur, producing
    -- an analogous stream of results.
    defer :: e (Moment e a) -> e a

    -- | An event that never occurs.
    never :: e a
    default never :: Monoid (e a) => e a
    never = mempty

    -- | An event that occurs when either of the given events occur,
    -- preserving the ordering of occurences.
    union :: e a -> e a -> e a
    default union :: Monoid (e a) => e a -> e a -> e a
    union = mappend

    -- | Filters the occurences of an event, preserving the order of
    -- occurences.
    filterJust :: e (Maybe a) -> e a

-- | @e@ is a type constructor for an event that implements 'subdivide'. This
-- requires that there be a dense ordering of times at which events can occur
-- (which is not the case if there are discrete time steps).
class Event e => EventSubdivide e where

    -- | Constructs two events that both occur when the given event occurs,
    -- preserving the ordering of occurences with respect to all
    -- existing events. Additionally, all occurences of the second event
    -- will occur after the corresponding occurence of the first event.
    subdivide :: e a -> (e a, e a)

-- | @f@ is a type constructor for a behavior in a reactive system where
-- @m@ is a monad containing a time-dependent value.
class (MonadFix' m, Applicative f) => Behavior m f where

    -- | Obtains the value of a behavior at a particular moment.
    sample :: f a -> m a

-- | A reactive system consisting of events of type @e@ and behaviors of
-- type @f@.
class (Event e, Behavior (Moment e) f) => Reactive e f
instance (Event e, Behavior (Moment e) f) => Reactive e f

-- | Tags an event with the value of a behavior at the time it occurs.
infixl 4 <@>
(<@>) :: (Reactive e f) => f (a -> b) -> e a -> e b
(<@>) x y = defer ((\v -> sample x <*> pure v) <$> y)

-- | Tags an event with the value of a behavior at the time it occurs.
infixl 4 <@
(<@) :: (Reactive e f) => f a -> e b -> e a
(<@) x y = const <$> x <@> y

-- | A reactive system where behaviors (@I e@) can depend on events (@e@).
-- This allows the system to hold time-varying state.
class Reactive e (I e) => ReactiveState e where

    -- | The type of behavior created when accumulating an event of type @e@.
    type I e :: * -> *

    -- | Constructs a behavior with the given initial value that changes in
    -- response to an event, starting at a particular moment.
    accumB :: a -> e (a -> a) -> Moment e (I e a)

-- | Constructs a behavior with the given initial value that changes in
-- response to an event.
stepper :: (ReactiveState e) => a -> e a -> Moment e (I e a)
stepper init = accumB init . (const <$>)

-- | A reactive system where all behaviors (@f@) change at discrete moments
-- (thus having a countable number of changes).
class Reactive (D f) f => ReactiveDiscrete f where

    -- | The type of event created when monitoring the changes of a
    -- behavior of type @f@.
    type D f :: * -> *

    -- | Constructs an event which occurs when the given behavior changes
    -- (and sometimes when it doesn't). Each occurence is guranteed to
    -- occur after the corresponding change, but before the next change.
    -- Occurences are tagged with the old and new values for the behavior.
    changes :: f a -> D f (a, a)

-- | A reactive system where signals of type @g@ can be dynamically switched
-- using behaviors (@f@).
class (Applicative f, Functor g) => ReactiveSwitch f g where

    -- | Constructs a signal which defers to the signal specified by the given
    -- behavior.
    switch :: f (g a) -> g a

-- | Constructs an event that occurs when the event specified by the
-- given behavior occurs.
switchE :: (ReactiveSwitch f e) => f (e a) -> e a
switchE = switch

-- | Constructs a behavior that takes the value of the behavior specified by
-- the given behavior.
switchB :: (ReactiveSwitch f f) => f (f a) -> f a
switchB = switch

-- | @m@ is a monad that allows the manipulation of a reactive system with
-- event type @e@.
class Monad m => MonadReactive e m | m -> e where

    -- | Constructs an event and an associated procedure to make it occur.
    spawnE :: m (e a, a -> m ())

    -- | Instantiates a time-dependent value with the current time.
    liftMoment :: Moment e a -> m a

    -- | Begins executing all procedures given by an event.
    execute :: e (m a) -> m (e a)

-- | Constructs a behavior and an associated procedure to change its value.
spawnB :: (ReactiveState e, MonadReactive e m)
    => a -> m (I e a, (a -> a) -> m ())
spawnB initial = do
    (event, change) <- spawnE
    behavior <- liftMoment $ accumB initial event
    return (behavior, change)
