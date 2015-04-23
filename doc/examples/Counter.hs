import Graphics.Oedel
import Control.Applicative
import Control.Reactive
import qualified Control.Reactive.IO as IO

main = displayHtmlWidget (inset $ setHeight 100 $ block center $
    declare ((\upE downE ->
        let add = (+ 1) <$ (upE :: IO.Event ())
            sub = (\x -> x - 1) <$ (downE :: IO.Event ())
        in accumB (0 :: Int) (add <> sub))
        <$> inp "up" <*> inp "down") (out "count") $
    (tightTextDyn id ((show :: Int -> String) <$> dyn (inp "count")) <>
    strongSpace 10 <> button (const "up") (out "up") <> strongSpace 10 <>
    button (const "down") (out "down")))
