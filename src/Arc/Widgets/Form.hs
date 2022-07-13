{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Widgets.Form where

import Arc.Util
import Data.Text (Text)
import Reflex.Dom hiding (Checkbox)

class Form f where
    formTitle :: Maybe Text
    formTitle = Nothing
    formSubtitle :: Maybe Text
    formSubtitle = Nothing
    formFields :: DomBuilder t m => m (Dynamic t f)

formField :: forall f t m. (FormField f, DomBuilder t m) => m (Dynamic t f)
formField = divClass "form-field" $ fieldInputElement @f

class FormField f where
    fieldRequired :: Bool
    fieldRequired = False
    fieldInputElement :: DomBuilder t m => m (Dynamic t f)

form :: forall f t m. (Form f, DomBuilder t m) => m (Dynamic t f)
form = el "form" $ do
    maybe blank (elClass "h2" "title" . text) $ formTitle @f
    maybe blank (divClass "subtitle" . text) $ formSubtitle @f
    formFields @f
