module Arc.Clay.Code where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Arc.Widgets.Code
import Clay hiding (grey)
import Clay.Stylesheet (key)
import Control.Monad (forM_)

instance ColourSchemeToken TokenType where
    backgroundColourScheme AlertTok = base08 . base16Default
    backgroundColourScheme ErrorTok = base08 . base16Default
    backgroundColourScheme _ = const transparent
    foregroundColourScheme CharTok = base08 . base16Default
    foregroundColourScheme KeywordTok = base0E . base16Default
    foregroundColourScheme DataTypeTok = base0A . base16Default
    foregroundColourScheme DecValTok = base09 . base16Default
    foregroundColourScheme BaseNTok = base09 . base16Default
    foregroundColourScheme FloatTok = base09 . base16Default
    foregroundColourScheme ConstantTok = base09 . base16Default
    foregroundColourScheme SpecialCharTok = base0F . base16Default
    foregroundColourScheme StringTok = base0B . base16Default
    foregroundColourScheme VerbatimStringTok = base0B . base16Default
    foregroundColourScheme SpecialStringTok = base0B . base16Default
    foregroundColourScheme ImportTok = base0D . base16Default
    foregroundColourScheme CommentTok = base03 . base16Default
    foregroundColourScheme DocumentationTok = base08 . base16Default
    foregroundColourScheme AnnotationTok = base0F . base16Default
    foregroundColourScheme CommentVarTok = base03 . base16Default
    foregroundColourScheme OtherTok = base05 . base16Default
    foregroundColourScheme FunctionTok = base0D . base16Default
    foregroundColourScheme VariableTok = base08 . base16Default
    foregroundColourScheme ControlFlowTok = base0E . base16Default -- TODO
    foregroundColourScheme OperatorTok = base05 . base16Default
    foregroundColourScheme BuiltInTok = base0D . base16Default
    foregroundColourScheme ExtensionTok = base05 . base16Default -- TODO
    foregroundColourScheme PreprocessorTok = base0A . base16Default
    foregroundColourScheme AttributeTok = base0A . base16Default
    foregroundColourScheme RegionMarkerTok = base05 . base16Default -- TODO
    foregroundColourScheme InformationTok = base05 . base16Default -- TODO
    foregroundColourScheme WarningTok = base08 . base16Default
    foregroundColourScheme AlertTok = base00 . base16Default
    foregroundColourScheme ErrorTok = base00 . base16Default
    foregroundColourScheme NormalTok = base05 . base16Default

-- foregroundColourScheme _ = undefined

code_ :: Css
code_ = do
    pre ? preStyle
    code ? codeStyle

preStyle :: Css
preStyle = do
    padding2 (em 0.5) (em 1)
    overflow auto
    ".code" & do
        withColourScheme $ backgroundColor . base00 . base16Default
        key "counter-reset" $ Value "line 0"
        display grid
        gridTemplateColumns [minContent, fr 1]
        -- grid - auto - rows : 1 em
        -- gridGap (em 0.3)
        ".line-number" ? do
            color $ setA 0.5 grey
            textAlign $ other $ Value "right"
            fontSize (pct 90)
            marginRight (em 1)
            before & do
                key "counter-increment" $ Value "line"
                content $ Content "counter(line)"

codeStyle :: Css
codeStyle = do
    self <> "*" ? userSelect selectText
    fontFamily ["Fira Code"] []
    forM_ [minBound @TokenType .. maxBound] $ \tt -> do
        class_ tt ? applyColourScheme tt
    class_ CommentTok ? do
        fontStyle italic
    class_ ErrorTok ? do
        textDecoration underline
    class_ ExtensionTok ? do
        fontWeight bold
    class_ KeywordTok ? do
        fontWeight bold
