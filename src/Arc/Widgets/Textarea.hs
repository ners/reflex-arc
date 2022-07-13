module Arc.Widgets.Textarea where

import Arc.Util
import Data.Default (Default)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Reflex.Dom hiding (TextArea)

data Textarea = Textarea
    { textareaId :: Maybe Text
    , textareaLabel :: Maybe Text
    , textareaPlaceholder :: Maybe Text
    , textareaDisabled :: Bool
    }

instance Default Textarea where
    def = Textarea Nothing Nothing Nothing False

instance WithId Textarea where
    idAttr = textareaId
    defId i = def{textareaId = Just i}

textarea :: DomBuilder t m => Textarea -> m (Dynamic t Text)
textarea Textarea{..} = do
    maybe blank (labelFor textareaId . text) textareaLabel
    i <-
        textAreaElement $
            def
                & textAreaElementConfig_elementConfig
                    . elementConfig_initialAttributes
                    .~ mkAttrs [("placeholder",) <$> textareaPlaceholder]
    return $ _textAreaElement_value i
