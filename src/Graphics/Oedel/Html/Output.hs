{-# LANGUAGE OverloadedStrings #-}
module Graphics.Oedel.Html.Output where

import Graphics.Oedel.Html.Base
import Graphics.Oedel.Html.Block (Block)
import qualified Graphics.Oedel.Html.Block as Block
import System.Directory (getTemporaryDirectory)
import System.IO (openTempFile, hClose)
import System.Process (system)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (hPutStr)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Monoid

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

-- | Opens a browser to display the given HTML block.
displayHtmlBlock :: Block -> IO ()
displayHtmlBlock block = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempDir "out.html"
    hPutStr handle (blockToHtml block)
    hClose handle
    system path -- TODO: cross platformness
    return ()
