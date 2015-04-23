{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Oedel.Html.Widget where

import Graphics.Oedel.Layout ((|||), (===))
import qualified Graphics.Oedel.Layout as Layout
import Graphics.Oedel.Html.Base
import Graphics.Oedel.Html.Flow (Flow, TextStyle)
import qualified Graphics.Oedel.Html.Flow as Flow
import Graphics.Oedel.Html.Block (Block)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid
import Control.Reactive
import Control.Applicative

-- | Describes the data in a post request.
type Post = Map String String

-- | Augments an HTML figure of type @q@ with the ability to interact with
-- the user and access an environment of type @a@. Widgets live in a
-- reactive system with event type @e@ and behavior type @f@.
data Widget e f q a = forall b. (Monoid b)
    => Widget (a -> (f (q b), e (Post, b) -> a))

-- | Runs a widget.
runWidget :: (Monoid a) => Widget e f q a
    -> (forall b. (e (Post, b) -> (a, f (q b))) -> r) -> r
runWidget (Widget f) inner = inner (\post ->
    let (fig, update) = f env
        env = update post
    in (env, fig))

-- | Converts a static figure into a widget.
augment :: (Applicative f, Monoid a) => q () -> Widget e f q a
augment fig = Widget $ const (pure fig, const mempty)

-- | Applies a function to the underlying figure for a widget.
decorate :: (Functor f) => (forall a. (Monoid a) => q a -> r a)
    -> Widget e f q a
    -> Widget e f r a
decorate f (Widget inner) = Widget $ \env ->
    let (innerFig, update) = inner env
    in (f <$> innerFig, update)

-- | Composes widgets by composing their underlying figures naturally.
compose :: (Reactive e f, Functor q, Functor p, Monoid a)
    => (forall a. (Monoid a) => q a -> p a -> r a)
    -> Widget e f q a
    -> Widget e f p a
    -> Widget e f r a
compose f (Widget x) (Widget y) = Widget $ \env ->
    let (xFig', xUpdate) = x env
        (yFig', yUpdate) = y env
        xFig = ((\v -> (Just v, Nothing)) <$>) <$> xFig'
        yFig = ((\v -> (Nothing, Just v)) <$>) <$> yFig'
        update post =
            xUpdate ((\(p, m) -> (p, fromJust $ fst m)) <$> post) <>
            yUpdate ((\(p, m) -> (p, fromJust $ snd m)) <$> post)
    in (f <$> xFig <*> yFig, update)

instance (Reactive e f, Monoid a) => Monoid (Widget e f Flow a) where
    mempty = augment mempty
    mappend = compose mappend
instance (Reactive e f, Monoid a) => Layout.Flow (Widget e f Flow a) where
    tight = decorate Layout.tight
instance (Reactive e f, Monoid a)
    => Layout.FlowText TextStyle (Widget e f Flow a) where
        text style = augment . Layout.text style
instance (Reactive e f, Monoid a)
    => Layout.FlowSpace Length (Widget e f Flow a) where
        strongSpace = augment . Layout.strongSpace
instance (Reactive e f, Monoid a) => Layout.Block (Widget e f Block a) where
    (|||) = compose (|||)
    (===) = compose (===)
    compact = decorate Layout.compact
instance (Reactive e f, Monoid a)
    => Layout.BlockSolid Color (Widget e f Block a) where
        solid = augment . Layout.solid
instance (Reactive e f, Monoid a)
    => Layout.BlockTrans (Widget e f Block a) where
        clear = augment Layout.clear
        over = compose Layout.over
instance (Reactive e f, Monoid a)
    => Layout.FlowToBlock Flow.Alignment
    (Widget e f Flow a) (Widget e f Block a) where
        block alignment = decorate (Layout.block alignment)
