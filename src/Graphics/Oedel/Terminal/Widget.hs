{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Graphics.Oedel.Terminal.Widget (
    Widget
) where

import Control.Reactive
import qualified Control.Reactive.IO as IO
import qualified Graphics.Oedel.Layout as Layout
import Graphics.Oedel.Style hiding (key)
import Graphics.Oedel.Terminal.Base
import Graphics.Oedel.Terminal.Input
import Graphics.Oedel.Terminal.Flow (Flow, TextStyle)
import Graphics.Oedel.Terminal.Block (Block)
import Graphics.Oedel.Terminal.Paint (runPaint)
import qualified Graphics.Oedel.Terminal.Block as Block
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Control.Concurrent
import Control.Monad.State
import Control.Applicative

-- | Identifies a key that can be assigned to an action.
type Key = Char

-- | Gives a unique name to a persistent thing (or group of things) in a
-- widget.
type Name = [Int]

-- | Gives a unique name to a group of things in a widget.
type Group = Name

-- | A context in which names can be created.
type NameGen = State (Int, Name)

-- | Generates a new name within the context of a 'NameGen'.
newName :: NameGen Name
newName = do
    (head, tail) <- get
    put (head + 1, tail)
    return $ reverse (head : tail)

-- | Encapsulates all requests from a widget to its environment.
data Request e = Request {

    -- | The initial key requests for the widget. Each distinct name
    -- represents a feature to which a key can be assigned. Each feature is
    -- paired with the keys it may potentially be assigned to, in order
    -- of preference.
    initialKeyRequest :: [(Name, [Key])],

    -- | Dynamically requests keys, with similar behavior to
    -- 'initialKeyRequest'.
    keyRequest :: e (Name, [Key]) }

instance Event e => Monoid (Request e) where
    mempty = Request {
        initialKeyRequest = [],
        keyRequest = never }
    mappend x y = Request {
        initialKeyRequest = initialKeyRequest x <> initialKeyRequest y,
        keyRequest = union (keyRequest x) (keyRequest y) }

-- | Encapsulates all responses from an environment to a widget.
data Response e = Response {

    -- | The initial assignments for requested keys. If a feature is assigned
    -- a key, the key will be returned along with an event that occurs when
    -- the key is pressed.
    initialKeyAssign :: Name -> Maybe (Key, e ()),

    -- | Responds to dynamic key assignments made by 'keyRequest' (and
    -- some that haven't, so make sure to check the name).
    keyAssign :: e (Name, Maybe (Key, e ())) }

-- | Augments a figure of type @q f@ with the ability to interact with the
-- user and access an environment of type @a@. Widgets live in a reactive
-- system with event type @e@ and behavior type @f@.
data Widget e (f :: * -> *) q a = Widget
    (NameGen (a -> (a, Request e, Response e -> q f)))

-- | Converts a static figure into a widget.
augment :: (Event e) => q f -> Widget e f q a
augment source = Widget $ return $ \env -> (env, mempty, const source)

-- | Applies a function to the underlying figure for a widget.
decorate :: (Event e) => (q f -> r f) -> Widget e f q a -> Widget e f r a
decorate f (Widget x) = Widget $ (\inner env ->
    let (nEnv, req, fig) = inner env
    in (nEnv, req, f . fig)) <$> x

-- | Composes widgets by composing their underlying figures naturally.
compose :: (Event e) => (q f -> p f -> r f)
    -> Widget e f q a -> Widget e f p a -> Widget e f r a
compose f (Widget x) (Widget y) = Widget $ (\xInner yInner env ->
    let (xEnv, xReq, xFig) = xInner env
        (yEnv, yReq, yFig) = yInner xEnv
    in (yEnv, xReq <> yReq, \res -> f (xFig res) (yFig res)))
    <$> x <*> y

instance Reactive e f => Monoid (Widget e f Flow a) where
    mempty = augment mempty
    mappend = compose mappend
instance Reactive e f => Layout.Flow (Widget e f Flow a) where
    tight = decorate Layout.tight
