module Control.Reactive.Test where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Reactive
import Control.Reactive.IO (newEvent, value)

test :: Test
test = testGroup "Reactive.IO" [
    testCase "simple accum" $ do
        (updateE, update) <- newEvent
        behavior <- accumB (0 :: Int) updateE
        cur <- value behavior
        assertEqual "Initial value" 0 cur
        update (+ 2)
        cur <- value behavior
        assertEqual "After add" 2 cur
        update (* 3)
        cur <- value behavior
        assertEqual "After multiplty" 6 cur]
