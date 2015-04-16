{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Oedel.Html.Base (
    ToCss (..),
    Length (..),
    VarLength (..),
    Color (..),
    Writer (..),
    runWriterFull,
    enclose,
    encloseFor
) where

import qualified Graphics.Oedel.Color as Oedel
import Data.Monoid
import Data.String
import Data.Text.Lazy.Builder (Builder)
import Data.Char (intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map

-- | @a@ is a type that can be converted to a CSS value.
class ToCss a where

    -- | Converts a value to its CSS representation.
    toCss :: a -> String

-- | Describes an orthogonal length in HTML.
newtype Length = Points Double
    deriving (Eq, Ord, Show, Num, Real)
instance Monoid Length where
    mempty = 0
    mappend = (+)
instance ToCss Length where
    toCss (Points 0) = "0"
    toCss (Points pts) = show pts ++ "pt"

-- | Describes an orthogonal length in HTML that may depend on the length
-- of the parent container along the same axis.
data VarLength = VarLength Length Double
instance Monoid VarLength where
    mempty = VarLength mempty 0
    mappend (VarLength ab ap) (VarLength bb bp) =
        VarLength (ab <> bb) (ap + bp)
instance ToCss VarLength where
    toCss (VarLength base 0) = toCss base
    toCss (VarLength 0 p) = show (p * 100.0) ++ "%"
    toCss (VarLength base p) = "calc(" ++ toCss base ++ " + " ++
        show (p * 100.0) ++ "%)"

-- | Describes a valid color in HTML.
data Color = Color Int Int Int
instance Oedel.Color Color where
    rgb r g b = Color (c r) (c g) (c b) where
        c x = min 255 $ ceiling (x * 256)
instance ToCss Color where
    toCss (Color r g b) = "#" ++ show r ++ show g ++ show b where
        show i = [h, l] where
            h = intToDigit (i `div` 16)
            l = intToDigit (i `rem` 16)

-- | Describes a CSS style.
type Style = Map String String

-- | Constructs an HTML element with the given tag name, style and contents.
buildElement :: String -> [(String, String)] -> Builder -> Builder
buildElement tag [] inner = "<" <> fromString tag <> ">" <> inner <>
    "</" <> fromString tag <> ">"
buildElement tag props inner = res where
    f (prop, val) = fromString prop <> ":" <> fromString val <> ";"
    res = "<" <> fromString tag <> " style=\"" <>
        foldr ((<>) . f) "" props <> "\">" <> inner <>
        "</" <> fromString tag <> ">"


-- | Produces an HTML string in an environment of styles and scripts.
data Writer = Writer {

    -- | The inheritable style attributes this writer would like to have in
    -- its context.
    requestStyle :: Style,

    -- | Produces output for this writer given the set of inheritable style
    -- attributes that are active in its context.
    runWriter :: Style -> Builder }

-- | Runs a writer to produce a full HTML document.
runWriterFull :: Writer -> Builder
runWriterFull inner =
    let style = requestStyle inner
        body = buildElement "body" (Map.toList style) $ runWriter inner style
    in "<html>" <> body <> "</html>"

instance IsString Writer where
    fromString str = Writer {
        requestStyle = Map.empty,
        runWriter = const $ fromString str }
instance Monoid Writer where
    mempty = Writer {
        requestStyle = Map.empty,
        runWriter = const mempty }
    mappend x y = Writer {
        requestStyle = Map.union (requestStyle x) (requestStyle y),
        runWriter = \style -> runWriter x style <> runWriter y style }

-- | Encloses the contents of a 'Writer' with an element of the given tag
-- and style.
enclose' :: String
    -> Maybe [(String, String)]   -- ^ immediate (non-inheritable) style
    -> [(String, String)]         -- ^ requested (inheritable) style
    -> Writer -> Writer
enclose' tag imm req' source =
    let req = Map.fromList req'
    in Writer {
        requestStyle = Map.union req (requestStyle source),
        runWriter = \style ->
            let missing = Map.differenceWith
                    (\req has -> if req == has then Nothing else Just req)
                    req style
            in case imm of
                Nothing -> if Map.null missing then runWriter source style
                    else buildElement tag (Map.toList req) $
                        runWriter source (Map.union missing style)
                Just imm -> buildElement tag (imm ++ Map.toList req) $
                    runWriter source (Map.union missing style) }

-- | Encloses the contents of a 'Writer' with an element of the given tag
-- and style.
enclose :: String
    -> [(String, String)]   -- ^ immediate (non-inheritable) style
    -> [(String, String)]   -- ^ requested (inheritable) style
    -> Writer -> Writer
enclose tag imm = enclose' tag (Just imm)

-- | Encloses the contents of a 'Writer' with an element of the given tag
-- and style, but only if needed to fufill the requested style.
encloseFor :: String -> [(String, String)] -> Writer -> Writer
encloseFor tag = enclose' tag Nothing
