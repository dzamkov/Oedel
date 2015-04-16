{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Oedel.Html.Flow where

import qualified Graphics.Oedel.Layout as Layout
import Graphics.Oedel.Attr
import Graphics.Oedel.Html.Base
import Data.String
import Data.Monoid
import Data.Maybe (catMaybes)
import Control.Applicative

-- | A constructor for a group of HTML elements that occur inline.
data Flow = Flow {

    -- | Indicates whether this flow has any natural spaces in it.
    hasSpace :: Bool,

    -- | Converts a flow into its HTML representation, excluding the enclosing
    -- style.
    renderInner :: Writer }

-- | Converts a flow into its HTML representation.
render :: Alignment -> Flow -> Writer
render alignment flow = encloseFor "span" (catMaybes [
    Just ("text-align", toCss alignment),
    if hasSpace flow then Just ("white-space", "pre-wrap") else Nothing]) $
    renderInner flow

instance Monoid Flow where
    mempty = Flow {
        hasSpace = False,
        renderInner = mempty }
    mappend x y = Flow {
        hasSpace = hasSpace x || hasSpace y,
        renderInner = renderInner x <> "&#8203;" <> renderInner y }
instance Layout.Flow Flow where
    tight source = source {
        renderInner = encloseFor "span" [("white-space", "pre")] $
            renderInner source }
instance Layout.FlowText TextStyle Flow where
    text style str = Flow {
        hasSpace = True,
        renderInner = encloseFor "span"
            (textStyleToCss $ style defaultStyle) $ fromString str }
instance Layout.FlowSpace Length Flow where
    strongSpace len = Flow {
        hasSpace = False,
        renderInner = enclose "span" [("width", toCss len)] [] "" }

-- | A possible style for text in a flow.
data TextStyle = TextStyle { textColor :: Maybe Color }
instance AttrColor Color TextStyle where
    color c style = style { textColor = Just c }
instance HasDefault TextStyle where
    defaultStyle = TextStyle { textColor = Nothing }

-- | Converts a 'TextStyle' to a list of CSS attributes.
textStyleToCss :: TextStyle -> [(String, String)]
textStyleToCss style = catMaybes [
    (\color -> ("color", toCss color)) <$> textColor style]

-- | A possible alignment for a flow.
newtype Alignment = Alignment String
instance Layout.Alignment Alignment where
    left = Alignment "left"
    center = Alignment "center"
    right = Alignment "right"
instance ToCss Alignment where
    toCss (Alignment value) = value
