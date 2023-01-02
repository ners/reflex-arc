{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Clay.Util where

import Arc.Util
import Clay hiding (s)
import Clay.Stylesheet (key, prefixed)
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (encodeUtf8)
import Data.Text.Lazy qualified as Text (toStrict)

renderText :: Css -> Text
renderText = Text.toStrict . render

renderBS :: Css -> ByteString
renderBS = Text.encodeUtf8 . renderText

borderRadiusAll :: Size a -> Css
borderRadiusAll s = borderRadius s s s s

marginAll :: Size a -> Css
marginAll s = margin s s s s

margin2 :: Size a -> Size a -> Css
margin2 y x = margin y x y x

paddingAll :: Size a -> Css
paddingAll s = padding s s s s

padding2 :: Size a -> Size a -> Css
padding2 y x = padding y x y x

squareSize :: Size a -> Css
squareSize s = width s <> height s

self :: Selector
self = ""

sibling :: Selector -> Selector
sibling = (self |~)

instance Semigroup Content where
    (Content c1) <> (Content c2) = Content $ c1 <> " " <> c2

instance Monoid Content where
    mempty = Content ""

formatContent :: Text -> Content
formatContent f = Content ("format(" <> value (Literal f) <> ")")

techContent :: Text -> Content
techContent t = Content ("tech(" <> value (Literal t) <> ")")

appearance :: Val v => v -> Css
appearance = prefixed (browsers <> "appearance")

tableDisplay :: Display
tableDisplay = Display "table"

inheritFont :: Required a
inheritFont = Required inherit Nothing [] []

class_ :: forall c s. (ClassName c, IsString s) => c -> s
class_ c = fromString $ "." <> className @c c

baseClass_ :: forall c s. (BaseClassName c, IsString s) => s
baseClass_ = fromString $ "." <> baseClassName @c

mdiFont :: Css
mdiFont = do
    fontFamily ["Material Design Icons"] []
    key "-webkit-font-smoothing" $ Value "antialiased"
    key "-moz-osx-font-smoothing" $ Value "grayscale"

charContent :: Char -> Content
charContent = stringContent . Text.singleton

cursorNone :: CursorValue Value
cursorNone = none
