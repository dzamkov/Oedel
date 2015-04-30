module Control.Reactive.Test where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Reactive hiding (spawnE)
import Control.Reactive.IO (spawnE, await, value)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Monoid

test :: Test
test = testGroup "IO FRP" [

    testCase "simple accum" $ do
        (updateE, update) <- spawnE
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
        (upE, up) <- spawnE
        (downE, down) <- spawnE
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

    testCase "long up counter" $ do
        (upE, up) <- spawnE
        behavior <- accumB (0 :: Int) ((+ 1) <$ upE)
        forM_ [(0 :: Int) .. 9999] $ const $ up ()
        cur <- value behavior
        assertEqual "Final count" 10000 cur,

    testCase "multi-threaded up counter" $ do
        (upEs, ups) <- unzip <$> forM [(0 :: Int) .. 99] (const spawnE)
        let upE = foldr1 union upEs
        behavior <- accumB (0 :: Int) ((+ 1) <$ upE)
        refs <- forM ups $ \up -> do
            ref <- newEmptyMVar
            forkIO $ do
                forM_ [(0 :: Int) .. 99] $ const $ threadDelay 1 >> up ()
                putMVar ref ()
            return ref
        forM_ refs takeMVar
        cur <- value behavior
        assertEqual "Final count" 10000 cur,

    testCase "sharing" $ do
        (updateE, update) <- spawnE
        behavior <- accumB (0 :: Integer) updateE
        let shared = iterate (\x -> (+) <$> x <*> x) behavior !! 100
        cur <- value shared
        assertEqual "Initial value" 0 cur
        update (const 1)
        cur <- value shared
        assertEqual "After set" (2 ^ (100 :: Integer)) cur,

    testCase "tag" $ do
        (updateE, update) <- spawnE
        (readE, read) <- spawnE
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
        (takeE, take) <- spawnE
        (giveE, give) <- spawnE
        forkIO $ do
            val <- await takeE
            give (val * 2)
        take 3
        val <- await giveE
        assertEqual "Final value" (6 :: Int) val]
