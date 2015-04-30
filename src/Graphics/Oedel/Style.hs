{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
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

-- | Uses the default style within an inner context.
withDefaultStyle :: (Style p) => ((?style :: p) => a) -> a
withDefaultStyle inner =
    let ?style = deft
    in inner

-- | Modifies the implicitly-passed style within an inner context.
withStyle :: (?style :: p) => (p -> p) -> ((?style :: p) => a) -> a
withStyle f inner =
    let curStyle = ?style
    in let ?style = f curStyle
    in inner

-- | @p@ is a styling description that allows a text color to be specified.
class (Color c, Style p) => AttrTextColor c p | p -> c where

    -- | Applies the given text color to a styling description
    textColor :: c -> p -> p

-- | @p@ is a styling description that allows a font family of type @f@ to
-- be specified.
class Style p => AttrFont f p | p -> f where

    -- | Applies the given font to a styling description.
    font :: f -> p -> p

-- | @p@ is a styling description that allows a font size to be specified.
class Style p => AttrFontSize h p | p -> h where

    -- | Applies the given font size to a styling description.
    fontSize :: h -> p -> p

-- | @p@ is a styling description that allows shortcut keys to be specified.
class AttrKey p where
    {-# MINIMAL keys | key #-}

    -- | Applies one of the given shortcut keys (in order of preference) to a
    -- styling description.
    keys :: [Char] -> p -> p
    keys = key . head

    -- | Applies the given shortcut key to a styling description.
    key :: Char -> p -> p
    key = keys . (: [])
