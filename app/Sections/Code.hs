{-# LANGUAGE TemplateHaskell #-}

module Sections.Code where

import Arc.Clay.Normalise (normalise)
import Arc.Clay.Util (renderText)
import Arc.Widgets.Code
import Data.FileEmbed (embedFile)
import Data.Text.Encoding qualified as Text
import Reflex.Dom

codeSection :: DomBuilder t m => m ()
codeSection = do
    codeBlock $
        def
            { codeBlockLanguage = Just "Haskell"
            , codeBlockContent = Text.decodeUtf8 $(embedFile "src/Arc/Clay/Normalise.hs")
            }
    codeBlock $
        def
            { codeBlockLanguage = Just "CSS"
            , codeBlockContent = renderText normalise
            }
