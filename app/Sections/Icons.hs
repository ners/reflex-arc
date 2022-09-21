module Sections.Icons where

import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Icon
import Control.Monad (forM_)
import qualified Data.Text as Text
import Reflex.Dom
import Web.Font.MDI

iconsSection :: DomBuilder t m => m ()
iconsSection = do
    forM_ [(tag, size) | tag <- ["h1", "h2", "h3", "div"], size <- [minBound .. maxBound]] $ \(tag, size) -> do
        el "hr" blank
        el "div" $ text $ Text.unwords [tag, tshow size]
        forM_
            [ (i, content)
            | i <- [mdiIcon mdiAccountCircle]
            , content <- [Nothing, Just "with text"]
            ]
            $ \(i, content) -> do
                el tag $ do
                    let icn = i{iconSize = size}
                    case content of
                        Just t -> iconWithText icn t
                        Nothing -> icon icn
                    el "span" $ text "surrounding text"
