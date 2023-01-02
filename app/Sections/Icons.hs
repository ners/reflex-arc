module Sections.Icons where

import Arc.Tokens.ArcLogo (arcLogoIcon)
import Arc.Util (tshow)
import Arc.Widgets.Icon
import Control.Monad (forM_)
import Data.Text qualified as Text
import Reflex.Dom hiding (tag)

iconsSection :: DomBuilder t m => m ()
iconsSection = do
    forM_ [(tag, size) | tag <- ["h1", "h2", "h3", "div"], size <- [minBound .. maxBound]] $ \(tag, size) -> do
        el "hr" blank
        el "div" $ text $ Text.unwords [tag, tshow size]
        forM_
            [ (i, content)
            | i <- [mdiIcon mdiAccountCircle, arcLogoIcon]
            , content <- [Nothing, Just "with text"]
            ]
            $ \(i, content) -> do
                el tag $ do
                    let icn = i{iconSize = size}
                    case content of
                        Just t -> iconWithText icn t
                        Nothing -> icon icn
                    el "span" $ text "surrounding text"
