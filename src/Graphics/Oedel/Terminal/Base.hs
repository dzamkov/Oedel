{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Oedel.Terminal.Base (
    Width (..), Height (..),
    IsLength (..),
    X, Y,
    Offset,
    Point,
    Color (..),
    Appearance,
    defaultAppearance
) where

import qualified Graphics.Oedel.Color as Oedel
import qualified System.Console.ANSI as ANSI
import Data.Monoid
import Data.List (minimumBy)
import Data.Function (on)

-- | Describes a horizontal length (number of cells) in the terminal.
newtype Width = Width Int
    deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Bounded)
instance Monoid Width where
    mempty = 0
    mappend = (+)

-- | Describes a vertical length (number of cells) in the terminal.
newtype Height = Height Int
    deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Bounded)
instance Monoid Height where
    mempty = 0
    mappend = (+)

-- | @a@ is a length in the context of a terminal.
class IsLength a where

    -- | Gets the number of cells in the given length.
    cells :: a -> Int

instance IsLength Width where
    cells (Width x) = x
instance IsLength Height where
    cells (Height x) = x

-- | Describes a horizontal offset in the terminal.
type X = Width

-- | Describes a vertical offset in the terminal.
type Y = Height

-- | Describes an offset in a terminal.
type Offset = (X, Y)

-- | Describes a point in a terminal.
type Point = Offset

-- | Describes a color in a terminal.
data Color = Color ANSI.ColorIntensity ANSI.Color
    deriving (Eq, Ord, Show)
instance Oedel.Color Color where
    rgb = approxRGB defaultRGBs

-- | The 16 terminal colors and their associated default RGB values.
defaultRGBs :: [(Double, Double, Double, Color)]
defaultRGBs = [
    (0.0, 0.0, 0.0, Color ANSI.Dull ANSI.Black),
    (0.5, 0.5, 0.5, Color ANSI.Vivid ANSI.Black),
    (0.5, 0.0, 0.0, Color ANSI.Dull ANSI.Red),
    (1.0, 0.0, 0.0, Color ANSI.Vivid ANSI.Red),
    (0.0, 0.5, 0.0, Color ANSI.Dull ANSI.Green),
    (0.0, 1.0, 0.0, Color ANSI.Vivid ANSI.Green),
    (0.5, 0.5, 0.0, Color ANSI.Dull ANSI.Yellow),
    (1.0, 1.0, 0.0, Color ANSI.Vivid ANSI.Yellow),
    (0.0, 0.0, 0.5, Color ANSI.Dull ANSI.Blue),
    (0.0, 0.0, 1.0, Color ANSI.Vivid ANSI.Blue),
    (0.5, 0.0, 0.5, Color ANSI.Dull ANSI.Magenta),
    (1.0, 0.0, 1.0, Color ANSI.Vivid ANSI.Magenta),
    (0.0, 0.5, 0.5, Color ANSI.Dull ANSI.Cyan),
    (0.0, 1.0, 1.0, Color ANSI.Vivid ANSI.Cyan),
    (0.75, 0.75, 0.75, Color ANSI.Dull ANSI.White),
    (1.0, 1.0, 1.0, Color ANSI.Vivid ANSI.White)]

-- | Gets the color in the given list that has the closest RGB representation
-- to the given color.
approxRGB :: [(Double, Double, Double, c)] -> Double -> Double -> Double -> c
approxRGB possible r g b =
    let sq x = x * x
        diff (r', g', b', _) = sq (r - r') + sq (g - g') + sq (b - b')
        (_, _, _, best) = minimumBy (compare `on` diff) possible
    in best

-- | Describes the appearance of a glyph on the terminal.
type Appearance = (Color, Color)

-- | The default appearance for some terminal.
defaultAppearance :: Appearance
defaultAppearance = (Color ANSI.Dull ANSI.Black, Color ANSI.Dull ANSI.White)
