module Arc.Widgets.Checkbox where

import Data.Text (Text)
import qualified Data.Text as Text

import Arc.Util
import Data.Default (Default)
import Reflex.Dom hiding (Checkbox)

data CheckboxType = CheckboxType | RadioType | SwitchType

instance ShowType CheckboxType where
    showType CheckboxType = "checkbox"
    showType RadioType = "radio"

data Checkbox = Checkbox
    { checkboxType :: CheckboxType
    , checkboxId :: Maybe Text
    , checkboxName :: Maybe Text
    , checkboxLabel :: Maybe Text
    , checkboxValue :: Maybe Text
    , checkboxDisabled :: Bool
    }

instance Default Checkbox where
    def = Checkbox CheckboxType Nothing Nothing Nothing Nothing False

instance WithId Checkbox where
    idAttr = checkboxId
    defId i = def{checkboxId = Just i}

checkbox :: DomBuilder t m => Checkbox -> m (Dynamic t Bool)
checkbox Checkbox{..} = do
    i <-
        inputElement $
            def
                & inputElementConfig_elementConfig
                    . elementConfig_initialAttributes
                    .~ mkAttrs
                        [ Just ("type", showType checkboxType)
                        , ("id",) <$> checkboxId
                        , ("name",) <$> checkboxName
                        , ("value",) <$> checkboxValue
                        , maybeDisabled checkboxDisabled
                        ]
    maybe blank (labelFor checkboxId . text) checkboxLabel
    return $ _inputElement_checked i
