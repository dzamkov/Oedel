import Graphics.Oedel
import Control.Applicative
import Control.Reactive
import qualified Control.Reactive.IO as IO

main :: IO ()
main = displayHtmlWidget $
    withDefaultTextStyle $
    withTextStyle (fontSize 20) $
    inset $ setHeight 100 $ block center $
    declare (out "count") ((\upE downE ->
        let add = (+ 1) <$ (upE :: IO.Event ())
            sub = (\x -> x - 1) <$ (downE :: IO.Event ())
        in accumB (0 :: Int) (add <> sub))
        <$> inp "up" <*> inp "down") $
    (tightTextDyn ((show :: Int -> String) <$> dyn (inp "count")) <>
    strongSpace 10 <> button (style $ content $ text "Up") (out "up") <>
    strongSpace 10 <> button (style $ content $ text "Down") (out "down"))
