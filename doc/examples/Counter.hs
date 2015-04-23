import Graphics.Oedel

main = displayHtmlWidget (inset $ setHeight 100 $ block center
    (button id (out "up") <> button id (out "down")))
