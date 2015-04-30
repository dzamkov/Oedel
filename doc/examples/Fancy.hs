import Graphics.Oedel

-- Styling example with simple layout structure.

main :: IO ()
main = displayHtmlStatic $
    setBack (rgb 0.8 0.9 1.0) $
    inset $ withBorder () $
    setBack white $
    setWidth 300 $ setHeight 90 $
    pad 10 10 10 10 $
    block center $
    withStyle (textColor (rgb 1.0 0.2 0.0)) $
    withStyle (font "cursive" . fontSize 50) $
    text "Fancy!"
