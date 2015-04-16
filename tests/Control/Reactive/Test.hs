module Control.Reactive.Test where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Reactive
import Control.Reactive.IO (newEvent, await, value)
import Control.Applicative
import Control.Concurrent

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
        assertEqual "After multiplty" 6 cur,

    testCase "sharing" $ do
        (updateE, update) <- newEvent
        behavior <- accumB (0 :: Integer) updateE
        let shared = iterate (\x -> (+) <$> x <*> x) behavior !! 100
        cur <- value shared
        assertEqual "Initial value" 0 cur
        update (const 1)
        cur <- value shared
        assertEqual "After set" (2 ^ (100 :: Integer)) cur,

    testCase "tag" $ do
        (updateE, update) <- newEvent
        (readE, read) <- newEvent
        source <- accumB (0 :: Int) updateE
        let addE = (\x _ y -> x + y) <$> source <@> readE
        res <- accumB (0 :: Int) addE
        update (const 1)
        read ()
        update (const 2)
        read ()
        cur <- value res
        assertEqual "Final value" 3 cur,

    testCase "await" $ do
        (takeE, take) <- newEvent
        (giveE, give) <- newEvent
        forkIO $ do
            val <- await takeE
            give (val * 2)
        take 3
        val <- await giveE
        assertEqual "Final value" (6 :: Int) val]
