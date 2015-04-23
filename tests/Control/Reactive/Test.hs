module Control.Reactive.Test where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Reactive
import Control.Reactive.IO (newEvent, await, value)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Monoid

test :: Test
test = testGroup "IO FRP" [

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

    testCase "up/down counter" $ do
        (upE, up) <- newEvent
        (downE, down) <- newEvent
        let change = ((+ 1) <$ upE) <> ((\x -> x - 1) <$ downE)
        behavior <- accumB (0 :: Int) change
        up () >> up ()
        cur <- value behavior
        assertEqual "for part 1: " 2 cur
        up () >> up ()
        cur <- value behavior
        assertEqual "for part 2: " 4 cur
        down () >> up ()
        cur <- value behavior
        assertEqual "for part 3: " 4 cur
        down () >> down () >> down ()
        up () >> up () >> down ()
        cur <- value behavior
        assertEqual "for part 4: " 2 cur,

    testCase "multi-threaded up counter" $ do
        (upE, up) <- newEvent
        behavior <- accumB (0 :: Int) ((+ 1) <$ upE)
        let up100 = forM_ [(0 :: Int) .. 99] $ const $ threadDelay 1 >> up ()
            forkUp100 = do
                ref <- newEmptyMVar
                forkIO $ do
                    up100
                    putMVar ref ()
                return ref
        refs <- forM [(0 :: Int) .. 99] $ const forkUp100
        forM_ refs takeMVar
        cur <- value behavior
        assertEqual "Final count" 10000 cur,

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
