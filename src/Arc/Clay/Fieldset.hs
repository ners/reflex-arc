module Arc.Clay.Fieldset where

import Arc.Clay.Util (self)
import Clay
import Clay.Stylesheet (key)

fieldset_ :: Css
fieldset_ = fieldset ? fieldsetStyle

fieldsetStyle :: Css
fieldsetStyle = do
    key "border" noneValue
    key "padding-block" nil
    key "margin-inline" nil
    key "padding-inline" nil
    self |> legend ? do
        width (pct 100)