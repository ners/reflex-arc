module Arc.Clay.App where

import Arc.Clay.Buttons
import Arc.Clay.Code
import Arc.Clay.Fieldset
import Arc.Clay.Fonts
import Arc.Clay.Form
import Arc.Clay.Icons
import Arc.Clay.Inputs
import Arc.Clay.Layouts
import Arc.Clay.Message (messages)
import Arc.Clay.Normalise
import Arc.Clay.Util
import Clay
import Clay.Stylesheet (key)

appStyle :: Css
appStyle = do
    normalise
    body ? bodyStyle
    fonts
    fieldset_
    code_
    forms
    icons
    messages
    layouts
    iconWithTexts
    inputs
    buttons

bodyStyle :: Css
bodyStyle = do
    fontSize $ px 16
    fontWeight $ weight 400
    fontFamily ["Source Sans Variable", "Source Sans 3 VF"] [sansSerif]
    key "font-stretch" $ Value "50%"
    lineHeight $ em 1.5
    display flex
    flexDirection column
    main_ ? mainStyle

mainStyle :: Css
mainStyle = do
    flexGrow 1
    alignItems stretch
    display flex
    flexDirection column
