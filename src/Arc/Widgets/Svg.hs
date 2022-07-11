module Arc.Widgets.Svg where

import Arc.Util
import Arc.Tokens.Size

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as Text

data ViewBox = ViewBox
    { minX :: Int
    , minY :: Int
    , width :: Int
    , height :: Int
    }

instance Show ViewBox where
    show (ViewBox {..}) = show minX <> " " <> show minY <> " " <> show width <> " " <> show height 

data Path = Path Text

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
        svgEl = elDynAttrNS 
            (Just xmlns)
            "svg" $ mkDynAttrs
                [ ("viewBox", tshow svgViewBox)
                , ("width", tshow svgWidth)
                , ("height", tshow svgHeight)
                , ("xmlns", xmlns)
                ]
        pathEl = elDynAttrNS (Just xmlns) "path" $ mkDynAttrs [("d", tshow svgPath), ("fill", "currentColor")]