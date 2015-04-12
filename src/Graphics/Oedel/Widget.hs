{-# LANGUAGE FunctionalDependencies #-}
module Graphics.Oedel.Widget where

import Data.Monoid
import Control.Reactive
import Control.Applicative

-- | Identifies an input of type @b@ within an environment of type @a@.
newtype Input a b = Input (a -> Maybe b)
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
newtype Output a b = Output (b -> a)
instance Monoid a => Monoid (Output a b) where
    mempty = Output $ const mempty
    mappend (Output x) (Output y) = Output $ \v -> x v <> y v

-- | @w a@ is a description of an interactive figure within an environment of
-- type @a@. Widgets can read from and write to their environment, and widgets
-- with the same environment type can be composed as figures.
class ReactiveState m e f => Widget m e f w | w -> m e f where

    -- | Decorates a widget to, upon instantiation, read the given input,
    -- instantiate it with the current time, and then write it to the given
    -- output.
    declare :: Input a (m b) -> Output a b -> w a -> w a

-- | @w@ is a widget type that allows dynamic switching.
class Widget m e f w => WidgetSwitch m e f w where

    -- | Constructs a widget whose contents dynamically switch between
    -- widgets. The initial widget is given, along with an event that selects
    -- other widgets.
    frame :: w a -> Input a (e (w a)) -> w a

-- | @w@ is a widget type that allows the construction of buttons.
class Widget m e f w => WidgetButton p m e f w | w -> p where

    -- | Constructs a button widget with the given style. The given output
    -- event will occur when the button is pressed.
    button :: (p -> p) -> Output a (e ()) -> w a
