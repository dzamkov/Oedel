{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}

import Graphics.Oedel
import Control.Applicative
import Control.Reactive
import qualified Control.Reactive.IO as IO
import Data.List

data Direction = None | Asc | Desc

numbers :: [Int]
numbers = [3, 5, 6, 2, 1, 7, 9, 4, 0, 5, 7]

response :: Direction -> Bool -> String
response dir dup = res where
    nNumbers' = case dir of
        None -> numbers
        Asc -> sort numbers
        Desc -> reverse $ sort numbers
    nNumbers = if dup then nNumbers' else nub nNumbers'
    res = foldr1 (\x y -> x ++ ", " ++ y) $ map show nNumbers

main :: IO ()
main = displayHtmlWidget $
    inset $ setHeight 100 $ block center $
    declare (out "response") ((\dirB (dupB :: IO.Behavior Bool) (goE :: IO.Event ()) ->
        accumB "" ((\dir dup _ _ -> response dir dup) <$> dirB <*> dupB <@> goE))
        <$> inp "dir" <*> inp "dup" <*> inp "go") $
    (text "sort " <> options (out "dir")
        [("none", None), ("ascending", Asc), ("descending", Desc)] <>
    strongSpace 10 <> text "allow duplicates" <> checkBox (out "dup") <>
    strongSpace 10 <> button (out "go") (text "Go") <>
    strongSpace 10 <> tightTextDyn (dyn (inp "response")))
