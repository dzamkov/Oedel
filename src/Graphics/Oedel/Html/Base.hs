{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Oedel.Html.Base where

import qualified Graphics.Oedel.Color as Oedel
import Data.Monoid
import Data.Text (pack)
import Data.Text.Lazy.Builder
import Data.Char (intToDigit)

-- | Describes an orthogonal length in HTML.
newtype Length = Points Double
    deriving (Eq, Ord, Show, Num, Real)
instance Monoid Length where
    mempty = 0
    mappend = (+)

-- | Converts a 'Length' to its CSS representation.
lengthToCss :: Length -> Builder
lengthToCss (Points 0) = "0"
lengthToCss (Points pts) = fromText (pack $ show pts) <> "pt"

-- | Describes an orthogonal length in HTML that may depend on the length
-- of the parent container along the same axis.
data VarLength = VarLength Length Double
instance Monoid VarLength where
    mempty = VarLength mempty 0
    mappend (VarLength ab ap) (VarLength bb bp) =
        VarLength (ab <> bb) (ap + bp)

-- | Converts a 'VarLength' to its CSS representation.
varLengthToCss :: VarLength -> Builder
varLengthToCss (VarLength base 0) = lengthToCss base
varLengthToCss (VarLength 0 p) = fromText (pack $ show $ p * 100.0) <> "%"
varLengthToCss (VarLength base p) = "calc(" <> lengthToCss base <> " + " <>
    fromText (pack $ show $ p * 100.0) <> "%)"

-- | Describes a valid color in HTML.
data Color = Color Int Int Int
instance Oedel.Color Color where
    rgb r g b = Color (c r) (c g) (c b) where
        c x = min 255 $ ceiling (x * 256)

-- | Converts a 'Color' to its CSS representation.
colorToCss :: Color -> Builder
colorToCss (Color r g b) = "#" <> show r <> show g <> show b where
    show i = fromText (pack [h, l]) where
        h = intToDigit (i `div` 16)
        l = intToDigit (i `rem` 16)

-- | Converts a CSS style into a single string builder.
cssStyle :: [(String, Builder)] -> Builder
cssStyle = foldr ((<>) . f) "" where
    f (name, value) = fromText (pack name) <> ":" <> value <> ";"
