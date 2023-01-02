module Arc.Widgets.Text where

import Arc.Util
import Data.Default (Default)
import Data.Text (Text)
import Reflex.Dom hiding (TextInput, textInput)

data TextInputSize = TextInputFull | TextInputInline
    deriving stock (Show)

instance ClassName TextInputSize

data TextInputType = TextInputType | EmailInputType | PasswordInputType

instance Show TextInputType where
    show TextInputType = "text"
    show EmailInputType = "email"
    show PasswordInputType = "password"

instance ClassName TextInputType

data TextInput = TextInput
    { textInputSize :: TextInputSize
    , textInputType :: TextInputType
    , textInputPlaceholder :: Maybe Text
    , textInputId :: Maybe Text
    , textInputLabel :: Maybe Text
    , textInputDisabled :: Bool
    }

instance ClassName TextInput where
    className TextInput{..} = className textInputSize

instance Default TextInput where
    def = TextInput TextInputFull TextInputType Nothing Nothing Nothing False

instance WithId TextInput where
    idAttr = textInputId
    defId i = def{textInputId = Just i}

textInput :: DomBuilder t m => TextInput -> m (Dynamic t Text)
textInput ti@TextInput{..} = do
    maybe blank (labelFor textInputId . text) textInputLabel
    i <-
        inputElement $
            def
                & inputElementConfig_elementConfig
                    . elementConfig_initialAttributes
                    .~ mkAttrs
                        [ Just ("type", tshow textInputType)
                        , Just ("class", className ti)
                        , ("id",) <$> textInputId
                        , ("placeholder",) <$> textInputPlaceholder
                        ]
    return $ _inputElement_value i

emailInput :: DomBuilder t m => TextInput -> m (Dynamic t Text)
emailInput tb = textInput $ tb{textInputType = EmailInputType}

passwordInput :: DomBuilder t m => TextInput -> m (Dynamic t Text)
passwordInput tb = textInput $ tb{textInputType = PasswordInputType}
