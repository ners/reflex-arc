module Arc.Clay.Layouts.Tabs where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Clay hiding (grey)

tabs :: Css
tabs = ".tabs" ? tabsStyle

tabsStyle :: Css
tabsStyle = do
    flexGrow 1
    display flex
    flexDirection column
    -- ".list" ?
    nav ? do
        ul ? do
            listStyleType none
            paddingAll nil
            marginAll nil
            marginTop (em 1)
            li ? do
                display inlineBlock
                borderBottom (em 0.2) solid transparent
                borderRadiusAll (em 0.2)
                ".selected" & do
                    fontWeight (FontWeight $ Value "500")
                    borderBottom (em 0.2) solid (setA 0.5 grey)
                a ? do
                    display inlineBlock
                    lineHeight (em 2)
                    padding2 nil (em 2)
                    textAlign center
                    borderRadiusAll (em 0.2)
                    hover & backgroundColor (setA 0.1 grey)
    ".detail" ? do
        flexGrow 1
