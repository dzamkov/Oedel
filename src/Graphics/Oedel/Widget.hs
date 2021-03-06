{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Oedel.Widget where

import Data.Monoid
import Graphics.Oedel.Style
import Control.Reactive
import Control.Applicative

-- | Identifies an input of type @b@ within an environment of type @a@.
newtype Input a b = Input {

    -- | Reads an input from an environment. If the input is not available,
    -- 'Nothing' will be returned.
    readEnv :: a -> Maybe b }

instance Functor (Input a) where
    fmap f (Input i) = Input $ (f <$>) . i
instance Applicative (Input a) where
    pure x = Input $ const $ Just x
    (<*>) (Input f) (Input x) = Input $ \e -> f e <*> x e
instance Monad (Input a) where
    return = pure
    (>>=) (Input x) f = Input $ \e -> x e >>= \v ->
        let Input y = f v
        in y e

-- | Identifies an output of type @b@ within an environment of type @a@.
newtype Output a b = Output {

    -- | Constructs an environment containing only the given output, set
    -- to the given value.
    putEnv :: b -> a }

instance Monoid a => Monoid (Output a b) where
    mempty = Output $ const mempty
    mappend (Output x) (Output y) = Output $ \v -> x v <> y v

-- | Composes 'Input' with a behavior-like applicative. This is meant to
-- be used with layout functions suffixed with "Dyn", allowing a layout
-- to vary based on a behavior embedded within an environment.
data InputDyn f a b = InputDyn (Input a (f b))
instance Functor f => Functor (InputDyn f a) where
    fmap f (InputDyn x) = InputDyn $ (f <$>) <$> x
instance Applicative f => Applicative (InputDyn f a) where
    pure x = InputDyn $ pure $ pure x
    (<*>) (InputDyn f) (InputDyn x) = InputDyn $ (<*>) <$> f <*> x

-- | Converts an 'Input' for a behavior into a 'InputDyn', for use in
-- layout functions suffixed with "Dyn".
dyn :: Input a (f b) -> InputDyn f a b
dyn = InputDyn

-- | Inverse of 'dyn'.
undyn :: InputDyn f a b -> Input a (f b)
undyn (InputDyn inp) = inp

-- | @w a@ is a description of an interactive figure within an environment of
-- type @a@. Widgets can read from and write to their environment, and widgets
-- with the same environment type can be composed as figures.
class ReactiveState e => Widget e w | w -> e where

    -- | Decorates a widget to, upon instantiation, read the given input,
    -- instantiate it with the current time, and then write it to the given
    -- output.
    declare :: (Monoid a) => Output a b -> Input a (Moment e b) -> w a -> w a

-- | @w@ is a widget type that allows dynamic switching.
class Widget e w => WidgetSwitch e w where

    -- | Constructs a widget whose contents dynamically switch between
    -- widgets. The initial widget is given, along with an event that selects
    -- other widgets.
    frame :: (Monoid a) => w a -> Input a (e (w a)) -> w a

-- | @w@ is a widget type that allows the construction of buttons
-- that can be styled with a description of type @p@.
class (Widget e w, Style p) => WidgetButton p e w where

    -- | Constructs a button widget enclosing the given widget.
    -- The given output event will occur when the button is pressed.
    button :: (?style :: p, Monoid a) => Output a (e ()) -> w a -> w a

-- | @w@ is a widget type that allows the construction of text boxes that
-- can be styled with a description of type @p@.
class (Widget e w, Style p) => WidgetTextBox p e w where

    -- | Constructs a text box. The given output behavior will provide the
    -- text box contents.
    textBox :: (?style :: p, Monoid a) => Output a (I e String) -> w a

-- | @w@ is a widget type that allows the construction of option selectors that
-- can be styled with a description of type @p@.
class (Widget e w, Style p) => WidgetOption p o e w | w -> o where

    -- | Constructs an option selector (combo box or radio buttons) displaying
    -- the given options. The given output behavior will provide the currently
    -- selected option.
    options :: (?style :: p, Monoid a) => Output a (I e b) -> [(o, b)] -> w a

-- | @w@ is a widget type that allows the construction of check boxes that
-- can be styled with a description of type @p@.
class (Widget e w, Style p) => WidgetCheckBox p e w where

    -- | Constructs a check box. The given output behavior will provide the
    -- current checked state.
    checkBox :: (?style :: p, Monoid a) => Output a (I e Bool) -> w a
