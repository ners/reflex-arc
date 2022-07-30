module Arc.Clay.App where

import Arc.Clay.Buttons
import Arc.Clay.Code
import Arc.Clay.Fieldset
import Arc.Clay.Fonts
import Arc.Clay.Form
import Arc.Clay.Icons
import Arc.Clay.Inputs
import Arc.Clay.Layouts
import Arc.Clay.Normalise
import Arc.Clay.Util
import Clay
import Clay.Stylesheet (key)

appStyle :: Css
appStyle = do
    normalise
    --star ? globalStyle
    body ? bodyStyle
    fonts
    fieldset_
    code_
    forms
    icons
    layouts
    iconWithTexts
    inputs
    buttons

globalStyle :: Css
globalStyle =
    star ? do
        key "appearance" $ Value "none"
        key "font" $ Value "inherit"
        key "color" $ Value "inherit"
        key "cursor" $ Value "inherit"
        key "highlight" $ Value "none"
        key "outline" $ Value "none"
        boxSizing borderBox

bodyStyle :: Css
bodyStyle = do
    fontSize $ px 16
    fontWeight $ weight 400
    fontFamily ["Source Sans Variable"] []
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
