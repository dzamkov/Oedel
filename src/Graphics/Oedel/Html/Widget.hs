{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Oedel.Html.Widget where

import Graphics.Oedel.Layout ((|||), (===))
import qualified Graphics.Oedel.Layout as Layout
import qualified Graphics.Oedel.Widget as Oedel
import Graphics.Oedel.Html.Base
import Graphics.Oedel.Html.Flow (Flow (..), TextStyle)
import qualified Graphics.Oedel.Html.Flow as Flow
import Graphics.Oedel.Html.Block (Block)
import Data.String
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Control.Reactive
import Control.Applicative

-- | Describes the data in a post request.
type Post = Map ByteString ByteString

-- | Augments an HTML figure of type @q@ with the ability to interact with
-- the user and access an environment of type @a@. Widgets live in a
-- reactive system with event type @e@ and behavior type @f@.
data Widget m e f q a = forall b. (Monoid b)
    => Widget (a -> m (f (q b), e (Post, b) -> a))

-- | Runs a widget.
runWidget :: (ReactiveState m e f, Monoid a) => Widget m e f q a
    -> (forall b. (e (Post, b) -> m (a, f (q b))) -> r) -> r
runWidget (Widget f) inner = inner (\post -> do
    (_, update') <- f mempty
    (_, update) <- f (update' post)
    let env = update post
    (fig, _) <- f env
    return (env, fig))

-- | Converts a static figure into a widget.
augment :: (ReactiveState m e f, Monoid a) => q () -> Widget m e f q a
augment fig = Widget $ const $ return (pure fig, const mempty)

-- | Applies a function to the underlying figure for a widget.
decorate :: (ReactiveState m e f)
    => (forall a. (Monoid a) => q a -> r a)
    -> Widget m e f q a
    -> Widget m e f r a
decorate f (Widget inner) = Widget $ \env -> do
    (innerFig, update) <- inner env
    return (f <$> innerFig, update)

-- | Composes widgets by composing their underlying figures naturally.
compose :: (ReactiveState m e f, Functor q, Functor p, Monoid a)
    => (forall a. (Monoid a) => q a -> p a -> r a)
    -> Widget m e f q a
    -> Widget m e f p a
    -> Widget m e f r a
compose f (Widget x) (Widget y) = Widget $ \env -> do
    (xFig', xUpdate) <- x env
    (yFig', yUpdate) <- y env
    let xFig = ((\v -> (Just v, Nothing)) <$>) <$> xFig'
        yFig = ((\v -> (Nothing, Just v)) <$>) <$> yFig'
        update post =
            xUpdate ((\(p, m) -> (p, fromJust $ fst m)) <$> post) <>
            yUpdate ((\(p, m) -> (p, fromJust $ snd m)) <$> post)
    return (f <$> xFig <*> yFig, update)

instance (ReactiveState m e f, Monoid a)
    => Monoid (Widget m e f Flow a) where
        mempty = augment mempty
        mappend = compose mappend
instance (ReactiveState m e f, Monoid a)
    => Layout.Flow (Widget m e f Flow a) where
        tight = decorate Layout.tight
instance (ReactiveState m e f, Monoid a)
    => Layout.FlowText TextStyle (Widget m e f Flow a) where
        text = augment . Layout.text
instance (ReactiveState m e f, Monoid a)
    => Layout.FlowTextDyn (Oedel.InputDyn f a)
    TextStyle (Widget m e f Flow a) where
        tightTextDyn inp = Widget $ \env ->
            return ((Layout.tightText :: String -> Flow ()) <$>
                fromMaybe (pure "") (Oedel.readEnv (Oedel.undyn inp) env),
                const mempty)
instance (ReactiveState m e f, Monoid a)
    => Layout.FlowSpace Length (Widget m e f Flow a) where
        strongSpace = augment . Layout.strongSpace
instance (ReactiveState m e f, Monoid a)
    => Layout.Block (Widget m e f Block a) where
        (|||) = compose (|||)
        (===) = compose (===)
        compact = decorate Layout.compact
instance (ReactiveState m e f, Monoid a)
    => Layout.BlockSize Length Length (Widget m e f Block a) where
        setWidth width = decorate (Layout.setWidth width)
        setHeight height = decorate (Layout.setHeight height)
instance (ReactiveState m e f, Monoid a)
    => Layout.BlockSolid Color (Widget m e f Block a) where
        solid = augment . Layout.solid
instance (ReactiveState m e f, Monoid a)
    => Layout.BlockTrans (Widget m e f Block a) where
        clear = augment Layout.clear
        over = compose Layout.over
instance (ReactiveState m e f, Monoid a)
    => Layout.FlowToBlock Flow.Alignment
    (Widget m e f Flow a) (Widget m e f Block a) where
        block alignment = decorate (Layout.block alignment)

instance (ReactiveState m e f)
    => Oedel.Widget m e f (Widget m e f q) where
        declare output input (Widget f) = Widget $ \env -> do
            (fig, update) <- f env
            nUpdate <- case Oedel.readEnv input env of
                Nothing -> return update
                Just cons -> do
                    value <- cons
                    return ((<> Oedel.putEnv output value) . update)
            return (fig, nUpdate)
instance (ReactiveState m e f)
    => Oedel.WidgetButton String m e f (Widget m e f Flow) where
        button style output = res where
            fig = Flow {
                Flow.hasSpace = False,
                Flow.renderInner = do
                    makeInteractive
                    name <- newName
                    "<button type=\"submit\" name=\"submit\" value=\"" <>
                        fromString name <> ("\">" :: Html ()) <>
                        fromString (style "") <> "</button>"
                    return (Just $ fromString name) }
            update post = Oedel.putEnv output $ filterJust $
                (\(post, mapping) ->
                    case (Map.lookup "submit" post, mapping) of
                        (Just val, Just name) | val == name -> Just ()
                        _ -> Nothing) <$> post
            res = Widget $ const $ return (pure fig, update)
