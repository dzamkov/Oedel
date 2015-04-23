module Graphics.Oedel.Widget.DynamicEnv (
    DynamicEnv,
    inp, out
) where

import Graphics.Oedel.Widget
import Data.Dynamic
import Data.Map
import qualified Data.Map as Map
import Data.Monoid

-- | An environment that dynamically stores a number of values, indexed
-- by an orderable key type.
newtype DynamicEnv a = DynamicEnv (Map a Dynamic)
instance Ord a => Monoid (DynamicEnv a) where
    mempty = DynamicEnv Map.empty
    mappend (DynamicEnv x) (DynamicEnv y) = DynamicEnv $ Map.union y x

-- | Constructs a keyed input for a dynamic environment.
inp :: (Ord a, Typeable b) => a -> Input (DynamicEnv a) b
inp key = Input $ \(DynamicEnv env) -> case Map.lookup key env of
    Just dyn -> fromDynamic dyn
    Nothing -> Nothing

-- | Constructs a keyed output for a dynamic environment.
out :: (Ord a, Typeable b) => a -> Output (DynamicEnv a) b
out key = Output $ \value -> DynamicEnv $ Map.singleton key (toDyn value)
