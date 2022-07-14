module Arc.Clay.Inputs.Textarea where

import Arc.Clay.Colours
import Arc.Clay.Inputs.Text
import Arc.Clay.Util
import Clay
import Clay.Stylesheet (key)

textareaStyle :: Css
textareaStyle = do
    textInputStyle
    display block
    key "resize" noneValue
    width $ pct 100
    minHeight $ em 5
