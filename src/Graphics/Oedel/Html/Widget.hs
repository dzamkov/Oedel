{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.Oedel.Html.Widget where

import Graphics.Oedel.Layout ((|||), (===))
import qualified Graphics.Oedel.Layout as Layout
import qualified Graphics.Oedel.Widget as Oedel
import Graphics.Oedel.Html.Base
import Graphics.Oedel.Html.Flow (Flow (..))
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
-- reactive system with event type @e@.
data Widget e q a = forall b. (Monoid b)
    => Widget (a -> Moment e (I e (q b), e (Post, b) -> a))

-- | Runs a widget.
runWidget :: (ReactiveState e, Monoid a) => Widget e q a
    -> (forall b. (e (Post, b) -> Moment e (a, I e (q b))) -> r) -> r
runWidget (Widget f) inner = inner (\post -> do
    (_, update') <- f mempty
    (_, update) <- f (update' post)
    let env = update post
    (fig, _) <- f env
    return (env, fig))

-- | Converts a static figure into a widget.
augment :: (ReactiveState e, Monoid a) => q () -> Widget e q a
augment fig = Widget $ const $ return (pure fig, const mempty)

-- | Applies a function to the underlying figure for a widget.
decorate :: (ReactiveState e)
    => (forall a. (Monoid a) => q a -> r a)
    -> Widget e q a
    -> Widget e r a
decorate f (Widget inner) = Widget $ \env -> do
    (innerFig, update) <- inner env
    return (f <$> innerFig, update)

-- | Composes widgets by composing their underlying figures naturally.
compose :: (ReactiveState e, Functor q, Functor p, Monoid a)
    => (forall a. (Monoid a) => q a -> p a -> r a)
    -> Widget e q a
    -> Widget e p a
    -> Widget e r a
compose f (Widget x) (Widget y) = Widget $ \env -> do
    (xFig', xUpdate) <- x env
    (yFig', yUpdate) <- y env
    let xFig = ((\v -> (Just v, Nothing)) <$>) <$> xFig'
        yFig = ((\v -> (Nothing, Just v)) <$>) <$> yFig'
        update post =
            xUpdate ((\(p, m) -> (p, fromJust $ fst m)) <$> post) <>
            yUpdate ((\(p, m) -> (p, fromJust $ snd m)) <$> post)
    return (f <$> xFig <*> yFig, update)

instance (ReactiveState e, Monoid a) => Monoid (Widget e Flow a) where
    mempty = augment mempty
    mappend = compose mappend
instance (ReactiveState e, Monoid a) => Layout.Flow (Widget e Flow a) where
        tight = decorate Layout.tight
instance (ReactiveState e, Monoid a) => Layout.FlowText (Widget e Flow a) where
    type TextStyle (Widget e Flow a) = Layout.TextStyle (Flow a)
    text = augment . Layout.text
instance (ReactiveState e, Monoid a, f ~ I e)
    => Layout.FlowTextDyn (Oedel.InputDyn f a) (Widget e Flow a) where
        tightTextDyn inp = Widget $ \env ->
            return ((Layout.tightText :: String -> Flow ()) <$>
                fromMaybe (pure "") (Oedel.readEnv (Oedel.undyn inp) env),
                const mempty)
instance (ReactiveState e, Monoid a)
    => Layout.FlowSpace Length (Widget e Flow a) where
        strongSpace = augment . Layout.strongSpace
instance (ReactiveState e, Monoid a)
    => Layout.Block (Widget e Block a) where
        (|||) = compose (|||)
        (===) = compose (===)
        compact = decorate Layout.compact
instance (ReactiveState e, Monoid a)
    => Layout.BlockSize Length Length (Widget e Block a) where
        setWidth width = decorate (Layout.setWidth width)
        setHeight height = decorate (Layout.setHeight height)
instance (ReactiveState e, Monoid a)
    => Layout.BlockSolid Color (Widget e Block a) where
        solid = augment . Layout.solid
instance (ReactiveState e, Monoid a)
    => Layout.BlockTrans (Widget e Block a) where
        clear = augment Layout.clear
        over = compose Layout.over
instance (ReactiveState e, Monoid a)
    => Layout.FlowToBlock Flow.Alignment
    (Widget e Flow a) (Widget e Block a) where
        block alignment = decorate (Layout.block alignment)

instance (ReactiveState e)
    => Oedel.Widget e (Widget e q) where
        declare output input (Widget f) = Widget $ \env -> do
            (fig, update) <- f env
            nUpdate <- case Oedel.readEnv input env of
                Nothing -> return update
                Just cons -> do
                    value <- cons
                    return ((<> Oedel.putEnv output value) . update)
            return (fig, nUpdate)
instance (ReactiveState e) => Oedel.WidgetButton e (Widget e Flow) where
    type ButtonStyle (Widget e Flow) = ()
    button output (Widget inner) = Widget $ \env -> do
        (innerFig, innerUpdate) <- inner env
        let fig = (\innerFig -> Flow {
                Flow.hasSpace = False,
                Flow.renderInner = do
                    makeInteractive
                    name <- newName
                    let inner = Flow.renderInner innerFig
                    innerMapping <- "<button type=\"submit\" " <>
                        "name=\"submit\" value=\"" <>
                        fromString name <> "\">" <>
                        noInherit inner <> "</button>"
                    return (innerMapping, Just $ fromString name) })
                <$> innerFig
            update' post = Oedel.putEnv output $ filterJust $
                (\(post, (_, mapping)) ->
                    case (Map.lookup "submit" post, mapping) of
                        (Just val, Just name) | val == name -> Just ()
                        _ -> Nothing) <$> post
            update post = update' post <>
                innerUpdate ((\(p, (m, _)) -> (p, m)) <$> post)
        return (fig, update)
