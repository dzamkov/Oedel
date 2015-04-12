module Graphics.Oedel.Terminal.Output where

import qualified Graphics.Oedel.Layout as Layout
import Graphics.Oedel.Terminal.Base
import Graphics.Oedel.Terminal.Draw
import Graphics.Oedel.Terminal.Paint
import Graphics.Oedel.Terminal.Flow (Flow)
import qualified Graphics.Oedel.Terminal.Flow as Flow
import Graphics.Oedel.Terminal.Block (Block)
import qualified Graphics.Oedel.Terminal.Block as Block
import Control.Monad.Identity
import Control.Applicative

-- | Draws a static flow to the current terminal, with a newline, simulating
-- REPL output.
printFlow :: Flow Identity -> IO ()
printFlow flow = runDrawInline (\width ->
    let alignment = Layout.left
        (Identity height, paint) = Flow.place flow alignment (pure width)
        context = pure (fst defaultAppearance, (0, 0))
    in (height, runIdentity $ fromPaint $ paint context))

-- | Draws a static block to the current terminal, simulating REPL output.
printBlock :: Block Identity -> IO ()
printBlock block = runDrawInline (\terminalWidth ->
    let (minWidth, minHeight, paint) = Block.place block width height
        width = max 6 <$> minWidth
        height = max 3 <$> minHeight
        context = pure (Just $ fst defaultAppearance, (0, 0))
    in (runIdentity height, runIdentity $ fromPaint $ paint context))

-- TODO: don't allow flows and blocks to be shrunk below their minimum size.
