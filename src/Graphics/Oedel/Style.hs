{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graphics.Oedel.Style where

import Graphics.Oedel.Color (Color)

{-# ANN module "HLint: ignore Use String" #-}

-- | @p@ is a styling description with a default value.
class Style p where

    -- | The default style for @p@.
    deft :: p

instance Style () where
    deft = ()

-- | Constructs a style by applying a function to the default style.
style :: (Style p) => (p -> p) -> p
style f = f deft

-- | @p@ is a styling description that allows a color of type @c@ to be
-- specified.
class (Color c, Style p) => AttrColor c p | p -> c where

    -- | Applies the given color to a styling description.
    color :: c -> p -> p

-- | @p@ is a styling description for text that allows a font family of type
-- @f@ to be specified.
class Style p => AttrFont f p | p -> f where

    -- | Applies the given font to a styling description.
    font :: f -> p -> p

-- | @p@ is a styling description for text that allows a font size
-- to be specified.
class Style p => AttrFontSize h p | p -> h where

    -- | Applies the given font size to a styling description.
    fontSize :: h -> p -> p

-- | @p@ is a styling description for an element which can be associated
-- a shortcut key.
class AttrKey p where
    {-# MINIMAL keys | key #-}

    -- | Applies one of the given keys (in order of preference) to a styling
    -- description.
    keys :: [Char] -> p -> p
    keys = key . head

    -- | Applies the given key to a styling description.
    key :: Char -> p -> p
    key = keys . (: [])
