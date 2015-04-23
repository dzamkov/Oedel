module Graphics.Oedel.Html.Base.Test where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import qualified Graphics.Oedel.Html.Base as Base
import Data.List (nub)

defaultNames :: Test
defaultNames = testGroup "default names" [

    testCase "uniqueness" $ do
        let subNames = take 100 Base.defaultNames
        assertBool "Duplicate names have been generated"
            (length subNames == length (nub subNames)),

    testCase "shortness" $ do
        let subNames = take 1000 Base.defaultNames
        assertBool "Names are too long"
            (maximum (map length subNames) <= 4)]

test :: Test
test = testGroup "HTML base" [defaultNames]
