{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.MDI where

import Data.Text (Text)
import Reflex.Dom

mkMdiIcon :: DomBuilder t m => Text -> Text -> m ()
mkMdiIcon viewBox path = svgEl
  where
    xmlns = "http://www.w3.org/2000/svg"
    svgEl = elAttrNS (Just xmlns) "svg" ("viewBox" =: viewBox) pathEl
    pathEl = elAttrNS (Just xmlns) "path" ("d" =: path) blank
    elAttrNS mns elementTag attrs child = snd <$> elAttrNS' mns elementTag attrs child
    elAttrNS' mns elementTag attrs =
        element elementTag $
            def
                & elementConfig_namespace .~ mns
                & initialAttributes .~ attrs

-- GENERATED BELOW THIS LINE
