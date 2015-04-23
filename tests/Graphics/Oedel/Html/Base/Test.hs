module Graphics.Oedel.Html.Base.Test where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Graphics.Oedel.Html.Base
import Data.List (nub)
import Control.Applicative

nameGen :: Test
nameGen = testGroup "NameGen" [

    testCase "uniqueness" $ do
        let names = runNameGen (sequence $ newName <$ [(0 :: Int) .. 100]) "_"
        assertBool "Duplicate names have been generated"
            (length names == length (nub names)),

    testCase "shortness" $ do
        let names = runNameGen (sequence $ newName <$ [(0 :: Int) .. 1000]) "_"
        assertBool "Generated names are too long"
            (maximum (map length names) <= 4)]

test :: Test
test = testGroup "HTML base" [nameGen]
