module Arc.Widgets.Svg where

import Arc.Tokens.Size
import Arc.Util

import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom

data ViewBox = ViewBox
    { minX :: Int
    , minY :: Int
    , width :: Int
    , height :: Int
    }

instance Show ViewBox where
    show (ViewBox{..}) = show minX <> " " <> show minY <> " " <> show width <> " " <> show height

newtype Path = Path Text

instance Show Path where
    show (Path p) = Text.unpack p

data Svg = Svg
    { svgWidth :: Maybe Int
    , svgHeight :: Maybe Int
    , svgViewBox :: ViewBox
    , svgPath :: Path
    }

svg :: DomBuilder t m => Svg -> m ()
svg Svg{..} = svgEl $ pathEl blank
  where
    xmlns = "http://www.w3.org/2000/svg"
    svgEl =
        elAttrNS
            (Just xmlns)
            "svg"
            $ mkAttrs
                [ Just ("viewBox", tshow svgViewBox)
                , ("width",) . tshow <$> svgWidth
                , ("height",) . tshow <$> svgHeight
                ]
    pathEl = elAttrNS (Just xmlns) "path" $ mkAttrs [Just ("d", tshow svgPath)]
