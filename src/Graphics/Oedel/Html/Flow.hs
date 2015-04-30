{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
module Graphics.Oedel.Html.Flow where

import qualified Graphics.Oedel.Layout as Layout
import Graphics.Oedel.Html.Base
import Data.String
import Data.Monoid
import Data.Maybe (catMaybes)
import Control.Applicative

-- | A constructor for a group of HTML elements that occur inline; produces
-- a value of type @a@ when rendered.
data Flow a = Flow {

    -- | Indicates whether this flow has any natural spaces in it.
    hasSpace :: Bool,

    -- | Converts a flow into its HTML representation, excluding the enclosing
    -- style.
    renderInner :: Html a }

-- | Converts a flow into its HTML representation.
render :: Alignment -> Flow a -> Html a
render alignment flow = encloseFor "span" (listToStyle $ catMaybes [
    Just ("text-align", toCss alignment),
    if hasSpace flow then Just ("white-space", "pre-wrap") else Nothing]) $
    renderInner flow

instance Functor Flow where
    fmap f flow = flow { renderInner = f <$> renderInner flow }
instance Monoid a => Monoid (Flow a) where
    mempty = Flow {
        hasSpace = False,
        renderInner = mempty }
    mappend x y = Flow {
        hasSpace = hasSpace x || hasSpace y,
        renderInner = renderInner x <> "&#8203;" <> renderInner y }
instance Monoid a => Layout.Flow (Flow a) where
    tight source = source {
        renderInner = encloseFor "span"
            (listToStyle [("white-space", "pre")]) $
            renderInner source }
instance Monoid a => Layout.FlowText Style (Flow a) where
    text str = Flow {
        hasSpace = True,
        renderInner =
            let style = isolateTextStyle ?style
            in encloseFor "span" style $ fromString str }
instance Monoid a => Layout.FlowSpace Length (Flow a) where
    strongSpace len = Flow {
        hasSpace = False,
        renderInner = enclose "span" (listToStyle [
            ("display", "inline-block"),
            ("width", toCss len)]) mempty "" }

-- | A possible alignment for a flow.
newtype Alignment = Alignment String
instance Layout.Alignment Alignment where
    left = Alignment "left"
    center = Alignment "center"
    right = Alignment "right"
instance ToCss Alignment where
    toCss (Alignment value) = value
