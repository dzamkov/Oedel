{-# LANGUAGE OverloadedStrings #-}
module Graphics.Oedel.Html.Output where

import Graphics.Oedel.Html.Base
import Graphics.Oedel.Html.Block (Block)
import qualified Graphics.Oedel.Html.Block as Block
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Socket
import System.Process (system)
import Control.Concurrent
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding
import Data.Monoid
import Control.Monad (void)

-- | Converts the given HTML block into a text HTML representation.
blockToHtml :: Block -> Text
blockToHtml block = res where
    absolute = Block.Absolute {
        Block.left = mempty,
        Block.top = mempty,
        Block.right = mempty,
        Block.bottom = mempty }
    position = Block.PAbsolute absolute
    res = toLazyText (runWriterFull $ Block.render block position)

-- | Creates and binds a socket (for TCP) for any port.
bindAny :: IO Socket
bindAny = do
    host <- inet_addr "127.0.0.1"
    let addr = SockAddrInet aNY_PORT host -- localhost
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock addr
    return sock

-- | Opens a browser to display the given static HTML block. This returns
-- immediately after the browser has successfully opened the page.
displayHtmlStatic :: Block -> IO ()
displayHtmlStatic block = do
    shutdown <- newEmptyMVar
    sock <- bindAny
    port <- socketPort sock
    listen sock 1
    let app _ res = do
            putMVar shutdown ()
            res $ responseLBS status200 [("Content-Type", "text/html")] $
                encodeUtf8 $ blockToHtml block
    forkIO $ runSettingsSocket defaultSettings sock app
    -- TODO: cross platformness
    forkIO $ void $ system $ "start http://127.0.0.1:" ++ show port ++ "/"
    takeMVar shutdown
    threadDelay (2 * 1000 * 1000) -- Finish pending requests
    -- TODO: Cleanup?
