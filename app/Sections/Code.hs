module Sections.Code where

import Arc.Widgets.Code
import Data.Text qualified as Text
import Reflex.Dom

codeSection :: DomBuilder t m => m ()
codeSection =
    codeBlock $
        def
            { codeBlockLanguage = Just "Haskell"
            , codeBlockContent =
                Text.unlines
                    [ "main :: IO ()"
                    , "main = putStrLn \"Hello, Haskell!\""
                    ]
            }
