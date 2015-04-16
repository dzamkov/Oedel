import Test.Framework (defaultMain)
import qualified Control.Reactive.Test as Reactive

main :: IO ()
main = defaultMain [Reactive.test]
