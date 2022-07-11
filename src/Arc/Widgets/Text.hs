module Arc.Widgets.Text where

import Arc.Util
import Data.Default (Default)
import Data.Text (Text)
import Reflex.Dom

data TextBoxSize = Full | Inline deriving (Show)

data TextBoxType = Text | Password deriving (Show)

data TextBox = TextBox
    { textBoxSize :: TextBoxSize
    , textBoxType :: TextBoxType
    , textBoxDisabled :: Bool
    }

instance Default TextBox where
    def = TextBox Full Text False

textBox :: (DomBuilder t m, PostBuild t m) => TextBox -> m (Dynamic t Text)
textBox TextBox{..} = do
    i <-
        inputElement $
            def
                & inputElementConfig_elementConfig
                    . elementConfig_initialAttributes
                .~ mkAttrs [("type", tshow textBoxType), ("class", tshow textBoxSize)]
    return $ _inputElement_value i
