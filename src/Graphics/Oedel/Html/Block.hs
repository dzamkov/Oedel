{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Oedel.Html.Block where

import Graphics.Oedel.Layout ((===), (|||))
import qualified Graphics.Oedel.Layout as Layout
import Graphics.Oedel.Html.Base
import Graphics.Oedel.Html.Flow (Flow)
import qualified Graphics.Oedel.Html.Flow as Flow
import Data.Monoid
import Control.Applicative

-- | Describes an absolute positioning of a block within a container.
data Absolute = Absolute {

    -- | The length from the block to the left edge of its container.
    left :: VarLength,

    -- | The length from the block to the top edge of its container.
    top :: VarLength,

    -- | The length from the block to the right edge of its container.
    right :: VarLength,

    -- | The length from the block to the bottom edge of its container.
    bottom :: VarLength }

-- | Describes a possible positioning of a block within a container.
data Position = PAbsolute Absolute

-- | Positions a block to fill its entire container.
dock :: Position
dock = PAbsolute Absolute {
    left = mempty,
    top = mempty,
    right = mempty,
    bottom = mempty }

-- | A constructor for a set of HTML elements that together occupy a
-- rectangular region; produces a value of type @a@ when rendered.
data Block a = Block {

    -- | The minimum width of the block.
    minWidth :: Length,

    -- | The minimum height of the block.
    minHeight :: Length,

    -- | Specifies, relative to other blocks, how much free width should
    -- be allocated to this block.
    freeWidth :: Rational,

    -- | Specifies, relative to other blocks, how much free height should
    -- be allocated to this block.
    freeHeight :: Rational,

    -- | Indicates whether this block can be given an immediate style directly.
    canStyle :: Bool,

    -- | Converts this block into an HTML representation using the given
    -- immediate style for the outermost element, if one exists.
    render :: Style -> Position -> Html a }

instance Functor Block where
    fmap f block = block { render = \s p -> f <$> render block s p }
instance Monoid a => Layout.Block (Block a) where
    (|||) l r =
        let tFreeWidth = freeWidth l + freeWidth r
            lF = fromRational (freeWidth l / tFreeWidth)
            rF = fromRational (freeWidth r / tFreeWidth)
        in Block {
            minWidth = minWidth l + minWidth r,
            minHeight = max (minHeight l) (minHeight r),
            freeWidth = tFreeWidth,
            freeHeight = min (freeHeight l) (freeHeight r),
            canStyle = False,
            render = \_ pos -> case pos of
                PAbsolute abs ->
                    let VarLength lB lP = left abs
                        VarLength rB rP = right abs
                        mW = minWidth
                        eB = (-lB) + (-rB) - mW l - mW r
                        eP = 1 - lP - rP
                        lAbs = abs { right = VarLength
                            (rB + mW r + Points rF * eB) (rP + rF * eP) }
                        rAbs = abs { left = VarLength
                            (lB + mW l + Points lF * eB) (lP + lF * eP) }
                    in render l mempty (PAbsolute lAbs) <>
                       render r mempty (PAbsolute rAbs) }
    (===) t b =
        let tFreeHeight = freeHeight t + freeHeight b
            tF = fromRational (freeHeight t / tFreeHeight)
            bF = fromRational (freeHeight b / tFreeHeight)
        in Block {
            minWidth = max (minWidth t) (minWidth b),
            minHeight = minHeight t + minHeight b,
            freeWidth = min (freeWidth t) (freeWidth b),
            freeHeight = tFreeHeight,
            canStyle = False,
            render = \_ pos -> case pos of
                PAbsolute abs ->
                    let VarLength tB tP = top abs
                        VarLength bB bP = bottom abs
                        mH = minHeight
                        eB = (-tB) + (-bB) - mH t - mH b
                        eP = 1 - tP - bP
                        tAbs = abs { bottom = VarLength
                            (bB + mH b + Points bF * eB) (bP + bF * eP) }
                        bAbs = abs { top = VarLength
                            (tB + mH t + Points tF * eB) (tP + tF * eP) }
                    in render t mempty (PAbsolute tAbs) <>
                       render b mempty (PAbsolute bAbs) }
    compact block = block {
        freeWidth = 0,
        freeHeight = 0 }
instance Monoid a => Layout.BlockSize Length Length (Block a) where
    setWidth _ block | freeWidth block == 0 = block
    setWidth width block = block {
        minWidth = max (minWidth block) width,
        freeWidth = 0 }
    setHeight _ block | freeHeight block == 0 = block
    setHeight height block = block {
        minHeight = max (minHeight block) height,
        freeHeight = 0 }
instance Monoid a => Layout.BlockSolid Color (Block a) where
    solid color = Block {
        minWidth = 0,
        minHeight = 0,
        freeWidth = 1,
        freeHeight = 1,
        canStyle = True,
        render = renderDiv "" .
            (listToStyle [("background-color", toCss color)] <>) }
instance Monoid a => Layout.BlockTrans (Block a) where
    clear = Block {
        minWidth = 0,
        minHeight = 0,
        freeWidth = 1,
        freeHeight = 1,
        canStyle = True,
        render = \style -> if isStyleEmpty style
            then const ""
            else renderDiv "" style }
    over hi lo = Block {
        minWidth = max (minWidth hi) (minWidth lo),
        minHeight = max (minHeight hi) (minHeight lo),
        freeWidth = min (freeWidth hi) (freeWidth lo),
        freeHeight = min (freeHeight hi) (freeHeight lo),
        canStyle = canStyle lo,
        render = \style pos -> render lo style pos <> render hi mempty pos }
instance Monoid a => Layout.BlockBorder () (Block a) where
    withBorder _ = withOuterStyle (listToStyle [
        ("border", "1px solid gray"),
        ("box-shadow", "0 0 10pt 0 gray")])   -- TODO: Customization
instance Monoid a => Layout.FlowToBlock Flow.Alignment (Flow a) (Block a) where
    block alignment flow = Block {
        minWidth = 0,
        minHeight = 0,
        freeWidth = 1,
        freeHeight = 1,
        canStyle = True,
        render = renderDiv $ Flow.render alignment flow }

-- | Renders a @div@ block with the given style and contents.
renderDiv :: Html a -> Style -> Position -> Html a
renderDiv inner style' position = res where
    style = (<> style') $ case position of
        PAbsolute abs -> listToStyle [
            ("position", "absolute"),
            ("left", toCss $ left abs),
            ("top", toCss $ top abs),
            ("right", toCss $ right abs),
            ("bottom", toCss $ bottom abs)]
    res = enclose "div" style mempty inner

-- | Ensures that the given block is put in an element of its own. The
-- resulting block will have 'canStyle' set to 'True'.
withContainer :: Block a -> Block a
withContainer block = block {
    canStyle = True,
    render = renderDiv (render block mempty dock) }

-- | Applies the given immediate style to a block.
withOuterStyle :: Style -> Block a -> Block a
withOuterStyle style block | canStyle block = block {
    render = render block . (style <>) }
withOuterStyle style block = withOuterStyle style (withContainer block)
