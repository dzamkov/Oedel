module Graphics.Oedel.Widget.DynamicEnv (
    DynamicEnv,
    inp, out
) where

import Graphics.Oedel.Widget
import Data.Dynamic
import Data.Monoid
import Control.Monad

-- | An environment that dynamically stores a number of values, indexed
-- by an orderable key type.
newtype DynamicEnv a = DynamicEnv [(a, Dynamic)]
instance Ord a => Monoid (DynamicEnv a) where
    mempty = DynamicEnv []
    mappend (DynamicEnv x) (DynamicEnv y) = DynamicEnv $ x <> y

-- | Constructs a keyed input for a dynamic environment.
inp :: (Ord a, Typeable b) => a -> Input (DynamicEnv a) b
inp key = Input $ \(DynamicEnv env) ->
    let find ((k, v) : rem) | k == key = mplus (fromDynamic v) (find rem)
        find (_ : rem) = find rem
        find [] = Nothing
    in find env

-- | Constructs a keyed output for a dynamic environment.
out :: (Ord a, Typeable b) => a -> Output (DynamicEnv a) b
out key = Output $ \value -> DynamicEnv [(key, toDyn value)]
