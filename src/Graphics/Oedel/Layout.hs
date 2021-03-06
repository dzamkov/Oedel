{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
module Graphics.Oedel.Layout where

import Graphics.Oedel.Color (Color)
import Graphics.Oedel.Style
import Data.Monoid
import Control.Applicative

-- | @a@ is a flow-like figure, a linear arrangment of items interspersed with
-- potential breakpoints. When applied to an area, the flow can be broken into
-- horizontal pieces in order to fit. The monoid instance of @a@ can be used
-- to concatenate flows with implicit breakpoints between them.
class Monoid a => Flow a where

    -- | Removes the potential breakpoints from a flow, ensuring that it will
    -- appear as an unbroken horizontal unit.
    tight :: a -> a

-- | @a@ is a flow-like figure to which horizontal space of a set absolute
-- width can be added. All spaces are by default "breaking" because
-- concatenation of flows created implicit breakpoints. 'tight' may be used
-- to create non-breaking spaces.
class (Monoid w, Flow a) => FlowSpace w a | a -> w where

    -- | Constructs a weak space of the given width, so called because it
    -- may vanish when it occurs adjacent to a break, regardless of how wide it
    -- is supposed to be.
    weakSpace :: w -> a
    weakSpace = strongSpace

    -- | Constructs a strong space of the given width. Unlike a 'weakSpace', it
    -- will appear with its full width regardless of where it occurs.
    strongSpace :: w -> a

-- | Alias for 'weakSpace'.
space :: (FlowSpace w a) => w -> a
space = weakSpace

-- | @a@ is a flow-like figure with a means of displaying text. The text
-- can be styled using a description of type @p@.
class (Style p, Flow a) => FlowText p a where
    {-# MINIMAL (tightText, naturalSpace) | text #-}

    -- | Constructs a figure displaying the given text with no internal
    -- breakpoints.
    tightText :: (?style :: p) => String -> a
    tightText = tight . text

    -- | A flow item corresponding to a space between words in text with
    -- the given styling description.
    naturalSpace :: (?style :: p) => a
    naturalSpace = text " "

    -- | Constructs a figure displaying the given text with natural breakpoints
    -- between each word.
    text :: (?style :: p) => String -> a
    text = breakSpace where
        breakWord a [] = tightText (reverse a)
        breakWord a (' ' : xs) = tightText (reverse a) <> breakSpace xs
        breakWord a (x : xs) = breakWord (x : a) xs
        breakSpace [] = mempty
        breakSpace (' ' : xs) = breakSpace xs
        breakSpace (x : xs) = naturalSpace <> breakWord [x] xs

-- | @a@ is a flow-like figure with a means of displaying text that varies
-- dynamically in a context of type @f@.
class (Applicative f, FlowText p a) => FlowTextDyn f p a | a -> f where

    -- | Constructs a figure displaying the given dynamic text with no internal
    -- breakpoints.
    tightTextDyn :: (?style :: p) => f String -> a

-- | @a@ is a block-like figure, appearing as a rectangle whose size may take a
-- range of values.
class Block a where

    -- | Places one block beside another, causing the heights to coincide.
    -- The method of distributing widths is undefined.
    infixl 3 |||
    (|||) :: a -> a -> a

    -- | Stacks one block on top of another, causing the widths to coincide.
    -- The method of distributing heights is undefined.
    infixl 2 ===
    (===) :: a -> a -> a

    -- | Removes as much space from as a block as possible without hiding
    -- any content.
    compact :: a -> a

-- | @a@ is a block-like figure which may be given an absolute size.
class Block a => BlockSize w h a | a -> w h where

    -- | Resolves the width of a block to be as close to the given value as
    -- possible without hiding any content.
    setWidth :: w -> a -> a

    -- | Resolves the height of a block to be as close to the given value as
    -- possible without hiding any content.
    setHeight :: h -> a -> a

-- | @a@ is a block-like figure that allows the construction of solid-color
-- blocks.
class (Color c, Block a) => BlockSolid c a | a -> c where

    -- | Constructs a solid-color block of the given color.
    solid :: c -> a

-- | @a@ is a block-like figure that may be partially transparent.
class Block a => BlockTrans a where

    -- | A completely transparent block with ambiguity in width and height.
    clear :: a

    -- | Places one block over another, causing the widths, heights, and
    -- positions to coincide. This has no effect if the first block is
    -- completely opaque and can fit entirely over the second.
    over :: a -> a -> a

-- | Surronds a block with a transparent border of variable size, allowing
-- the inner and outer sizes to be vary independently.
inset :: (BlockTrans a) => a -> a
inset inner = clear === clear ||| inner ||| clear === clear

-- | Applies padding to a block, given the size of the padding in the left,
-- top, right and bottom directions.
pad :: (BlockSize w h a, BlockTrans a) => w -> h -> w -> h -> a -> a
pad l' t' r' b' inner = res where
    l = setWidth l' clear
    t = setHeight t' clear
    r = setWidth r' clear
    b = setHeight b' clear
    res = t === l ||| inner ||| r === b

-- | Sets the color of the transparent portions of a block.
setBack :: (BlockSolid c a, BlockTrans a) => c -> a -> a
setBack color hi = over hi $ solid color

-- | @a@ is a block-like figure to which a border of type @b@ may be applied.
class Block a => BlockBorder b a | a -> b where

    -- | Applies a border, with the given style, to a block.
    withBorder :: p -> a -> a

-- | @l@ is a possible alignment for lines within a flow.
class Alignment l where

    -- | Lines are aligned to the left of their container.
    left :: l

    -- | Lines are aligned in the center of their container.
    center :: l

    -- | Lines are aligned to the right of their container.
    right :: l

-- | @a@ is a 'Flow' figure that can be converted into a 'Block' figure of
-- type @b@.
class (Alignment l, Flow a, Block b)
    => FlowToBlock l a b | a -> l b, b -> l a where

        -- | Converts a flow into a translucent block using the given
        --alignment.
        block :: l -> a -> b
