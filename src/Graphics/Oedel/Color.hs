module Graphics.Oedel.Color where

-- | @c@ is a color, containing brightness information, but not transparency.
class Color c where

    -- | Gets the best approximation of the given RGB color. Components may
    -- range from @0.0@ to @1.0@.
    rgb :: Double -> Double -> Double -> c

-- | The closest approximation to black.
black :: (Color c) => c
black = rgb 0 0 0

-- | Gets the closest approximation to a gray with the given relative
-- luminosity between @0.0@ and @1.0@.
gray :: (Color c) => Double -> c
gray l = rgb l l l

-- | The closest approximation to white.
white :: (Color c) => c
white = rgb 1 1 1

-- | The closest approximation to red.
red :: (Color c) => c
red = rgb 1 0 0

-- | The closest approximation to green.
green :: (Color c) => c
green = rgb 0 1 0

-- | The closest approximation to blue.
blue :: (Color c) => c
blue = rgb 0 0 1

-- | The closest approximation to yellow.
yellow :: (Color c) => c
yellow = rgb 1 1 0

-- | The closest approximation to magenta.
magenta :: (Color c) => c
magenta = rgb 1 0 1

-- | The closest approximation to cyan.
cyan :: (Color c) => c
cyan = rgb 0 1 1
