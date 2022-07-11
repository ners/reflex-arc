module Arc.Clay.Util where

import Clay
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Lazy as Text (toStrict)

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

padding2 :: Size a -> Size a -> Css
padding2 y x = padding y x y x

squareSize :: Size a -> Css
squareSize s = width s <> height s

self :: Selector
self = ""

sibling :: Selector -> Selector
sibling = (self |~)
