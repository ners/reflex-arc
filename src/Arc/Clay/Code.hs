module Arc.Clay.Code where

import Arc.Clay.Util
import Clay

code_ :: Css
code_ = do
    pre ? preStyle
    code ? codeStyle

preStyle :: Css
preStyle = return ()

codeStyle :: Css
codeStyle = do
    fontFamily ["Source Code Pro"] []
    ".dt" ? color "#6f42c1"
    ".dv" ? color "#da6c11"
    ".cf" ? do
        fontWeight bold
        color "#565680"
    ".bn" ? color "#098aa5"
    ".fu" ? color "#565680"
    ".kw" ? do
        fontWeight bold
        color "#565680"
    ".pr" ? color "#b93b47"
    ".sy" ? color "#b9266f"
    ".va" ? color "#565680"
    ".cr" ? color "#6f42c1"
    ".op" ? color "#565680"
    ".ch" ? color "#098aa5"
    ".st" ? color "#158e6a"
    ".it" ? color "#da6c11"
    ".ra" ? color "#da6c11"
    ".co" ? color "#6c757d"
