{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Oedel.Html.Base (
    ToCss (..),
    Length (..),
    VarLength (..),
    Color (..),
    Style,
    isStyleEmpty,
    listToStyle,
    styleToList,
    isolateTextStyle,
    Name,
    defaultNames,
    Html,
    runHtmlFull,
    makeInteractive,
    newName,
    noInherit,
    enclose,
    encloseFor
) where

import qualified Graphics.Oedel.Color as OC
import Graphics.Oedel.Style hiding (Style)
import qualified Graphics.Oedel.Style as OS
import Data.Monoid
import Data.String
import Data.Text.Lazy.Builder (Builder)
import Data.Char (intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative

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
instance OC.Color Color where
    rgb r g b = Color (c r) (c g) (c b) where
        c x = min 255 $ ceiling (x * 256)
instance ToCss Color where
    toCss (Color r g b) = "#" ++ show r ++ show g ++ show b where
        show i = [h, l] where
            h = intToDigit (i `div` 16)
            l = intToDigit (i `rem` 16)

-- | Describes a CSS style.
newtype Style = Style (Map String String)
instance OS.Style Style where
    deft = Style Map.empty
instance Monoid Style where
    mempty = Style Map.empty
    mappend (Style x) (Style y) = Style $ Map.union x y
instance AttrTextColor Color Style where
    textColor color (Style x) = Style $ Map.insert "color" (toCss color) x
instance AttrFontSize Length Style where
    fontSize size (Style x) = Style $ Map.insert "font-size" (toCss size) x
instance AttrFont String Style where
    font name (Style x) = Style $ Map.insert "font-family" name x

-- | Determines whether a style is empty.
isStyleEmpty :: Style -> Bool
isStyleEmpty (Style x) = Map.null x

-- | Converts a list to a style.
listToStyle :: [(String, String)] -> Style
listToStyle = Style . Map.fromList

-- | Converts a style into a list.
styleToList :: Style -> [(String, String)]
styleToList (Style x) = Map.toList x

-- | Extracts the portion of a style that is related to text.
isolateTextStyle :: Style -> Style
isolateTextStyle = id -- TODO

-- | Constructs an HTML element with the given tag name, style and contents.
buildElement :: String -> [(String, String)] -> Builder -> Builder
buildElement tag [] inner = "<" <> fromString tag <> ">" <> inner <>
    "</" <> fromString tag <> ">"
buildElement tag props inner = res where
    f (prop, val) = fromString prop <> ":" <> fromString val <> ";"
    res = "<" <> fromString tag <> " style=\"" <>
        foldr ((<>) . f) "" props <> "\">" <> inner <>
        "</" <> fromString tag <> ">"

-- | A unique identifier for a script, style or HTML component.
type Name = String

-- | An infinite list of non-empty alphanumeric (with underscores) 'Name's.
defaultNames :: [Name]
defaultNames =
    let suffixes = do
            tail <- "" : suffixes
            ch <- ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
            return $ ch : tail
    in ("_" ++) <$> suffixes

-- | The state information kept by 'Html'.
data HtmlState = HtmlState {

    -- | An infinite list of unused names.
    names :: [Name],

    -- | Indicates whether the produced HTML content contains "input" tags.
    interactive :: Bool,

    -- | The inheritable style attributes that the 'Html' would like to
    -- have in its context.
    requestStyle :: Style }

-- | A procedure that runs in an environment of HTML styles, scripts and
-- components and produces HTML content.
newtype Html a = Html (State HtmlState (Style -> Builder, a))
instance Functor Html where
    fmap f (Html x) = Html $ (f <$>) <$> x
instance Applicative Html where
    pure x = Html $ pure (const mempty, x)
    (<*>) (Html f) (Html g) = Html $ (\(fc, fv) (gc, gv) ->
        (fc <> gc, fv gv)) <$> f <*> g
instance Monad Html where
    return = pure
    (>>=) (Html x) f = Html $ do
        (xc, xv) <- x
        let Html y = f xv
        (yc, yv) <- y
        return (xc <> yc, yv)
instance Monoid a => IsString (Html a) where
    fromString str = Html $ return (const $ fromString str, mempty)
instance Monoid a => Monoid (Html a) where
    mempty = pure mempty
    mappend x y = (<>) <$> x <*> y

-- | Runs an 'Html' to produce a full HTML document.
runHtmlFull :: Html a -> (Builder, a)
runHtmlFull (Html inner) =
    let initialState = HtmlState {
            names = defaultNames,
            interactive = False,
            requestStyle = mempty }
        ((build, value), state) = runState inner initialState
        content' = build $ requestStyle state
        content = if interactive state
            then "<form method=\"POST\">" <> content' <> "</form>"
            else content'
        body = buildElement "body" (styleToList $ requestStyle state) content
        document = "<html>" <> body <> "</html>"
    in (document, value)

-- | An 'Html' procedure that makes the HTML context interactive.
makeInteractive :: Html ()
makeInteractive = Html $ do
    modify (\state -> state { interactive = True })
    return (const mempty, ())

-- | Gets a new unique name within the context of an 'Html'.
newName :: Html Name
newName = Html $ do
    state <- get
    let (name : rem) = names state
        nState = state { names = rem }
    put nState
    return (const mempty, name)

-- | Disallows an 'Html' from inheriting styles from its containing context.
noInherit :: Html a -> Html a
noInherit (Html inner) = Html $ do
    state <- get
    let ((buildInner, value), final) = runState inner state
    put $ final { requestStyle = deft }
    return (const $ buildInner deft, value)

-- | Encloses the contents of an 'Html' with an element of the given tag
-- and style.
enclose' :: String
    -> Maybe Style        -- ^ immediate (non-inheritable) style
    -> Style              -- ^ requested (inheritable) style
    -> Html a -> Html a
enclose' tag imm req (Html inner) = Html $ do
    state <- get
    let innerState = state { requestStyle = deft }
        ((buildInner, value), final) = runState inner innerState
        innerReq = requestStyle final
        totalReq = req <> innerReq
        curReq = requestStyle state
    put final { requestStyle = curReq <> totalReq }
    let build style =
            let Style totalReqMap = totalReq
                Style styleMap = style
                missing = Map.differenceWith
                    (\req has -> if req == has then Nothing else Just req)
                    totalReqMap styleMap
            in case imm of
                Nothing -> if Map.null missing then buildInner style
                    else buildElement tag (Map.toList missing) $
                        buildInner (Style $ Map.union missing styleMap)
                Just (Style immMap) ->
                    buildElement tag (Map.toList $ Map.union immMap missing) $
                    buildInner (Style $ Map.union missing styleMap)
    return (build, value)

-- | Encloses the contents of a 'Html' with an element of the given tag
-- and style.
enclose :: String
    -> Style              -- ^ immediate (non-inheritable) style
    -> Style              -- ^ requested (inheritable) style
    -> Html a -> Html a
enclose tag imm = enclose' tag (Just imm)

-- | Encloses the contents of a 'Writer' with an element of the given tag
-- and style, but only if needed to fufill the requested style.
encloseFor :: String -> Style -> Html a -> Html a
encloseFor tag = enclose' tag Nothing
