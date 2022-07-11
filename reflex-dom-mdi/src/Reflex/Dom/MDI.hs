{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.MDI where

import Data.Text (Text)
import Reflex.Dom

mkMdiIcon :: (DomBuilder t m, PostBuild t m) => Text -> Text -> m ()
mkMdiIcon viewBox path = svgEl
  where
    xmlns = "http://www.w3.org/2000/svg"
    svgEl = elDynAttrNS (Just xmlns) "svg" (constDyn $ "viewBox" =: viewBox) pathEl
    pathEl = elDynAttrNS (Just xmlns) "path" (constDyn $ "d" =: path) blank

-- GENERATED BELOW THIS LINE
