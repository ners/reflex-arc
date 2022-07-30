module Arc.Clay.Code where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Arc.Widgets.Code
import Clay
import Clay.Media (maxWidth, screen)
import Clay.Stylesheet (Feature (..), MediaType (..), rule)
import qualified Skylighting as S

instance ColourSchemeToken S.TokenType where
    backgroundColourScheme S.AlertTok = base08 . base16Default
    backgroundColourScheme _ = const transparent
    foregroundColourScheme S.AlertTok = base05 . base16Default
    foregroundColourScheme S.AnnotationTok = base04 . base16Default
    foregroundColourScheme S.AttributeTok = base0D . base16Default
    foregroundColourScheme S.BaseNTok = base09 . base16Default
    foregroundColourScheme S.BuiltInTok = base0D . base16Default
    foregroundColourScheme S.CharTok = base0C . base16Default
    foregroundColourScheme S.CommentTok = base03 . base16Default
    foregroundColourScheme S.CommentVarTok = base0C . base16Default
    foregroundColourScheme S.ConstantTok = base09 . base16Default
    foregroundColourScheme S.ControlFlowTok = base09 . base16Default
    foregroundColourScheme S.DataTypeTok = base0A . base16Default
    foregroundColourScheme S.DecValTok = base09 . base16Default
    foregroundColourScheme S.DocumentationTok = base08 . base16Default
    foregroundColourScheme S.ErrorTok = base08 . base16Default
    foregroundColourScheme S.ExtensionTok = base0D . base16Default
    foregroundColourScheme S.FloatTok = base09 . base16Default
    foregroundColourScheme S.FunctionTok = base0D . base16Default
    foregroundColourScheme S.ImportTok = base0F . base16Default
    foregroundColourScheme S.InformationTok = base0C . base16Default
    foregroundColourScheme S.KeywordTok = base0E . base16Default
    foregroundColourScheme S.OperatorTok = base05 . base16Default
    foregroundColourScheme S.OtherTok = base0A . base16Default
    foregroundColourScheme S.PreprocessorTok = base0B . base16Default
    foregroundColourScheme S.RegionMarkerTok = base07 . base16Default
    foregroundColourScheme S.SpecialCharTok = base0C . base16Default
    foregroundColourScheme S.SpecialStringTok = base0F . base16Default
    foregroundColourScheme S.StringTok = base0B . base16Default
    foregroundColourScheme S.VariableTok = base08 . base16Default
    foregroundColourScheme S.VerbatimStringTok = base0B . base16Default
    foregroundColourScheme S.WarningTok = base0A . base16Default

code_ :: Css
code_ = do
    pre ? preStyle
    code ? codeStyle

preStyle :: Css
preStyle = do
    padding2 (em 0.5) (em 1)
    ".code" & do
        withColourScheme $ backgroundColor . base00 . base16Default

codeStyle :: Css
codeStyle = do
    let colourClass s = class_ s ? applyColourScheme s
    fontFamily ["Source Code Pro"] []
    mapM_
        colourClass
        [ S.AnnotationTok
        , S.AttributeTok
        , S.BaseNTok
        , S.BuiltInTok
        , S.CharTok
        , S.CommentTok
        , S.CommentVarTok
        , S.ConstantTok
        , S.ControlFlowTok
        , S.DataTypeTok
        , S.DecValTok
        , S.DocumentationTok
        , S.ErrorTok
        , S.ExtensionTok
        , S.FloatTok
        , S.FunctionTok
        , S.ImportTok
        , S.InformationTok
        , S.KeywordTok
        , S.OperatorTok
        , S.OtherTok
        , S.PreprocessorTok
        , S.RegionMarkerTok
        , S.SpecialCharTok
        , S.SpecialStringTok
        , S.StringTok
        , S.VariableTok
        , S.VerbatimStringTok
        , S.WarningTok
        ]
    class_ S.CommentTok ? do
        fontStyle italic
    class_ S.ErrorTok ? do
        textDecoration underline
    class_ S.ExtensionTok ? do
        fontWeight bold
    class_ S.KeywordTok ? do
        fontWeight bold
