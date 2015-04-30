{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.Oedel
import Control.Applicative
import Control.Reactive
import qualified Control.Reactive.IO as IO
import Data.Char

ordinal :: Int -> String
ordinal 1 = "1st"
ordinal 2 = "2nd"
ordinal 3 = "3rd"
ordinal n = show n ++ "th"

-- | Determines whether two strings are equivalent using a case-insensitive
-- comparison
(=~) :: String -> String -> Bool
(=~) x y = map toLower x == map toLower y

planets :: [String]
planets = [
    "Mercury",
    "Venus",
    "Earth",
    "Mars",
    "Jupiter",
    "Saturn",
    "Uranus",
    "Neptune"]

response :: String -> String
response input = case filter ((=~ input) . snd) $ zip [1 ..] planets of
    (i, name) : _ -> name ++ " is the " ++ ordinal i ++ " planet from the sun"
    [] -> input ++ " is not a planet"

main :: IO ()
main = displayHtmlWidget $
    withDefaultTextStyle $
    withDefaultTextBoxStyle $
    withDefaultButtonStyle $
    declare (out "response") ((\(inputB :: IO.Behavior String) (checkE :: IO.Event ()) ->
        accumB "" ((\input _ _ -> response input) <$> inputB <@> checkE))
        <$> inp "input" <*> inp "check") $
    block center $
    (textBox (out "input") <> button (out "check") (text "Check") <>
    tightTextDyn (dyn $ inp "response"))
