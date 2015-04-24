{-# LANGUAGE OverloadedStrings #-}
module Graphics.Oedel.Html.Output where

import Graphics.Oedel.Html.Base
import Graphics.Oedel.Html.Block (Block)
import qualified Graphics.Oedel.Html.Block as Block
import Graphics.Oedel.Html.Widget
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Socket
import System.Process (system)
import Control.Concurrent
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map as Map
import Data.Monoid
import Control.Reactive ((<@>))
import Control.Reactive.IO
import Control.Applicative
import Control.Monad (void)

-- | Converts the given HTML block into a text HTML representation.
blockToHtml :: Block a -> (Text, a)
blockToHtml block = res where
    absolute = Block.Absolute {
        Block.left = mempty,
        Block.top = mempty,
        Block.right = mempty,
        Block.bottom = mempty }
    position = Block.PAbsolute absolute
    (builder, value) = runHtmlFull $ Block.render block position
    res = (toLazyText builder, value)

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
displayHtmlStatic :: Block () -> IO ()
displayHtmlStatic block = do
    shutdown <- newEmptyMVar
    sock <- bindAny
    port <- socketPort sock
    listen sock 1
    let app _ res = do
            putMVar shutdown ()
            res $ responseLBS status200 [("Content-Type", "text/html")] $
                encodeUtf8 $ fst $ blockToHtml block
    forkIO $ runSettingsSocket defaultSettings sock app
    -- TODO: cross platformness
    forkIO $ void $ system $ "start http://127.0.0.1:" ++ show port ++ "/"
    takeMVar shutdown
    threadDelay (2 * 1000 * 1000) -- Finish pending requests
    -- TODO: Cleanup?

-- | Parses a post request.
parsePost :: ByteString -> Post
parsePost = Map.fromList .
    map (\field ->
        let [name, value] = ByteString.split '=' field
        in (name, value)) .
    ByteString.split '&'

-- | Opens a browser to display the given HTML widget and allow the user
-- to interact with it. This returns when the user pressed "enter"
-- in the terminal.
displayHtmlWidget :: (Monoid a) => Widget IO Event Behavior Block a -> IO ()
displayHtmlWidget widget = do
    sock <- bindAny
    port <- socketPort sock
    (post, makePost) <- newEvent
    app <- runWidget widget (\givePost -> do
        (mapping, setMapping) <- newBehavior (error "POST before GET")
        (_, fig) <- givePost $ flip (,) <$> mapping <@> post
        let app req res = case requestMethod req of
                "GET" -> do
                    block <- value fig
                    let (html, nMapping) = blockToHtml block
                    setMapping $ const nMapping
                    res $ responseLBS status200
                        [("Content-Type", "text/html")]
                        (encodeUtf8 html)
                "POST" -> do
                    body <- requestBody req
                    makePost $ parsePost body
                    res $ responseLBS status302
                        [("Location", "/")]
                        ""
                _ -> error "unknown method"
        return app)
    listen sock 1
    forkIO $ runSettingsSocket defaultSettings sock app
    forkIO $ void $ system $ "start http://127.0.0.1:" ++ show port ++ "/"
    void getLine
