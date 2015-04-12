{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Oedel.Html.Block where

import Graphics.Oedel.Layout ((===), (|||))
import qualified Graphics.Oedel.Layout as Layout
import Graphics.Oedel.Html.Base
import Data.Text.Lazy.Builder
import Data.Monoid

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

-- | Describes a possible positioning of a block within a container.+
data Position = PAbsolute Absolute

-- | A constructor for an HTML element that occupies a rectangular region.
data Block = Block {

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

    -- | Converts this block into an HTML tag representation.
    render :: Position -> Builder }

-- | Renders a @div@ block with the given style and contents.
renderDiv :: [(String, Builder)] -> Builder -> Position -> Builder
renderDiv style' inner position = res where
    style = (<> style') $ case position of
        PAbsolute abs -> [
            ("position", "absolute"),
            ("left", varLengthToCss $ left abs),
            ("top", varLengthToCss $ top abs),
            ("right", varLengthToCss $ right abs),
            ("bottom", varLengthToCss $ bottom abs)]
    res = "<div style=\"" <> cssStyle style <> "\">" <> inner <> "</div>"

instance Layout.Block Block where
    (|||) l r =
        let tFreeWidth = freeWidth l + freeWidth r
            lF = fromRational (freeWidth l / tFreeWidth)
            rF = fromRational (freeWidth r / tFreeWidth)
        in Block {
            minWidth = minWidth l + minWidth r,
            minHeight = max (minHeight l) (minHeight r),
            freeWidth = tFreeWidth,
            freeHeight = min (freeHeight l) (freeHeight r),
            render = \pos -> case pos of
                PAbsolute abs ->
                    let VarLength lB lP = left abs
                        VarLength rB rP = right abs
                        mW = minWidth
                        eB = (-lB) + (-rB) - mW l - mW r
                        eP = 1 - lP - rP
                        innerP = 1 - lP - rP
                        lAbs = abs { right = VarLength
                            (rB + mW r + (Points rF) * eB) (rP + rF * eP) }
                        rAbs = abs { left = VarLength
                            (lB + mW l + (Points lF) * eB) (lP + lF * eP) }
                    in render l (PAbsolute lAbs) <>
                       render r (PAbsolute rAbs) }
    (===) t b =
        let tFreeHeight = freeHeight t + freeHeight b
            tF = fromRational (freeHeight t / tFreeHeight)
            bF = fromRational (freeHeight b / tFreeHeight)
        in Block {
            minWidth = max (minWidth t) (minWidth b),
            minHeight = minHeight t + minHeight b,
            freeWidth = min (freeWidth t) (freeWidth b),
            freeHeight = tFreeHeight,
            render = \pos -> case pos of
                PAbsolute abs ->
                    let VarLength tB tP = top abs
                        VarLength bB bP = bottom abs
                        mH = minHeight
                        eB = (-tB) + (-bB) - mH t - mH b
                        eP = 1 - tP - bP
                        tAbs = abs { bottom = VarLength
                            (bB + mH b + (Points bF) * eB) (bP + bF * eP) }
                        bAbs = abs { top = VarLength
                            (tB + mH t + (Points tF) * eB) (tP + tF * eP) }
                    in render t (PAbsolute tAbs) <>
                       render b (PAbsolute bAbs) }
    compact block = block {
        freeWidth = 0,
        freeHeight = 0 }
instance Layout.BlockSize Length Length Block where
    setWidth _ block | freeWidth block == 0 = block
    setWidth width block = block {
        minWidth = max (minWidth block) width,
        freeWidth = 0 }
    setHeight _ block | freeHeight block == 0 = block
    setHeight height block = block {
        minHeight = max (minHeight block) height,
        freeHeight = 0 }
instance Layout.BlockSolid Color Block where
    solid color = Block {
        minWidth = 0,
        minHeight = 0,
        freeWidth = 1,
        freeHeight = 1,
        render = renderDiv [("background-color", colorToCss color)] "" }
instance Layout.BlockTrans Block where
    clear = Block {
        minWidth = 0,
        minHeight = 0,
        freeWidth = 1,
        freeHeight = 1,
        render = const "" }
    over hi lo = Block {
        minWidth = max (minWidth hi) (minWidth lo),
        minHeight = max (minHeight hi) (minHeight lo),
        freeWidth = min (freeWidth hi) (freeWidth lo),
        freeHeight = min (freeHeight hi) (freeHeight lo),
        render = \pos -> render lo pos <> render hi pos }
