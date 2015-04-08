module Graphics.Oedel.Color where

-- | @c@ is a color, containing brightness information, but not transparency.
class Color c where

    -- | Gets the best approximation of the given RGB color. Components may
    -- range from @0.0@ to @1.0@.
    rgb :: Double -> Double -> Double -> c

-- | The closest approximation to red.
red :: (Color c) => c
red = rgb 1 0 0

-- | The closest approximation to green.
green :: (Color c) => c
green = rgb 0 1 0

-- | The closest approximation to blue.
blue :: (Color c) => c
blue = rgb 0 0 1
