import Test.Framework (defaultMain)
import qualified Control.Reactive.Test as Reactive
import qualified Graphics.Oedel.Html.Base.Test as Base

main :: IO ()
main = defaultMain [Reactive.test, Base.test]
