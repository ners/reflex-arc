{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Widgets.Form where

import Arc.Util
import Data.Text (Text)
import Reflex.Dom hiding (Checkbox)

class FormField f where
    fieldRequired :: Bool
    fieldRequired = False
    fieldInputElement :: (DomBuilder t m, PostBuild t m) => m (Dynamic t f)

formField :: forall f t m. (FormField f, DomBuilder t m, PostBuild t m) => m (Dynamic t f)
formField = divClass "form-field input-group" $ fieldInputElement @f

checkboxFormField :: forall f t m. (FormField f, DomBuilder t m, PostBuild t m) => m (Dynamic t f)
checkboxFormField = divClass "form-field input-group checkbox" $ fieldInputElement @f

class Form f where
    formTitle :: Maybe Text
    formTitle = Nothing
    formSubtitle :: Maybe Text
    formSubtitle = Nothing
    formFields :: (DomBuilder t m, PostBuild t m) => m (Dynamic t f)

form :: forall f t m. (Form f, DomBuilder t m, PostBuild t m) => m (Dynamic t f)
form = elAttr "form" ("onsubmit" =: "return false") $ do
    maybe blank (elClass "h2" "title" . text) $ formTitle @f
    maybe blank (divClass "subtitle" . text) $ formSubtitle @f
    formFields @f
