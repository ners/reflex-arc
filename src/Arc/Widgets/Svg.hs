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
    { svgWidth :: Size
    , svgHeight :: Size
    , svgViewBox :: ViewBox
    , svgPath :: Path
    }

svg :: (DomBuilder t m, PostBuild t m) => Svg -> m ()
svg Svg{..} = svgEl $ pathEl blank
  where
    xmlns = "http://www.w3.org/2000/svg"
    svgEl =
        elDynAttrNS
            (Just xmlns)
            "svg"
            $ mkDynAttrs
                [ Just ("viewBox", tshow svgViewBox)
                , Just ("width", tshow svgWidth)
                , Just ("height", tshow svgHeight)
                , Just ("xmlns", xmlns)
                ]
    pathEl = elDynAttrNS (Just xmlns) "path" $ mkDynAttrs [Just ("d", tshow svgPath)]
