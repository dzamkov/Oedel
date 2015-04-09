{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Oedel.Html.Block where

import Graphics.Oedel ((===), (|||))
import qualified Graphics.Oedel as Oedel
import Graphics.Oedel.Html.Base
import Data.Text.Lazy.Builder
import Control.Applicative
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

    -- | The minimum width of the block, if known.
    minWidth :: Maybe Length,

    -- | The minimum height of the block, if known.
    minHeight :: Maybe Length,

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

instance Oedel.Block Block where
    (|||) x y = undefined -- TODO
    (===) x y = undefined -- TODO
    compact _ = undefined -- TODO
instance Oedel.BlockSolid Color Block where
    solid color = Block {
        minWidth = Just 0,
        minHeight = Just 0,
        freeWidth = 1,
        freeHeight = 1,
        render = renderDiv [("background-color", colorToCss color)] "" }
instance Oedel.BlockTrans Block where
    clear = Block {
        minWidth = Just 0,
        minHeight = Just 0,
        freeWidth = 1,
        freeHeight = 1,
        render = const "" }
    over hi lo = Block {
        minWidth = min <$> minWidth hi <*> minWidth lo,
        minHeight = min <$> minHeight hi <*> minHeight lo,
        freeWidth = min (freeWidth hi) (freeWidth lo),
        freeHeight = min (freeHeight hi) (freeHeight lo),
        render = \pos -> render lo pos <> render hi pos }
