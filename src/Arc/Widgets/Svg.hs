{-# OPTIONS_GHC -Wno-partial-fields #-}

module Arc.Widgets.Svg where

import Arc.Util
import Data.Default (Default)
import Data.Text (Text)
import Data.Text qualified as Text
import Reflex.Dom

data ViewBox = ViewBox
    { minX :: Int
    , minY :: Int
    , width :: Int
    , height :: Int
    }

instance Show ViewBox where
    show (ViewBox{..}) = show minX <> " " <> show minY <> " " <> show width <> " " <> show height

data SvgElement
    = Path
        { fill :: Maybe Text
        , d :: [[Text]]
        }
    | Polygon
        { fill :: Maybe Text
        , points :: [(Double, Double)]
        }

data Svg = Svg
    { svgWidth :: Maybe Int
    , svgHeight :: Maybe Int
    , svgViewBox :: ViewBox
    , svgElements :: [SvgElement]
    }

instance Default Svg where
    def = Svg Nothing Nothing (ViewBox 0 0 0 0) []

svg :: forall t m. DomBuilder t m => Svg -> m ()
svg Svg{..} = svg' $ mapM_ svgEl svgElements
  where
    xmlns = "http://www.w3.org/2000/svg" :: Text
    svg' =
        elAttrNS
            (Just xmlns)
            "svg"
            $ mkAttrs
                [ Just ("viewBox", tshow svgViewBox)
                , ("width",) . tshow <$> svgWidth
                , ("height",) . tshow <$> svgHeight
                ]

    mkStyle :: SvgElement -> Maybe (Text, Text)
    mkStyle e = (\f -> ("style", "fill:" <> f)) <$> fill e

    svgEl :: SvgElement -> m ()
    svgEl p@Path{..} = elAttrNS (Just xmlns) "path" (mkAttrs [Just ("d", d'), mkStyle p]) blank
      where
        d' = Text.intercalate " " $ Text.intercalate "," <$> d
    svgEl p@Polygon{..} = elAttrNS (Just xmlns) "polygon" (mkAttrs [Just ("points", Text.unwords points'), mkStyle p]) blank
      where
        points' = (\(x, y) -> tshow x <> "," <> tshow y) <$> points
