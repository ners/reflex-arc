module Arc.Clay.Code where

import Arc.Clay.Util
import Arc.Widgets.Code
import Clay
import Clay.Media (maxWidth, screen)
import Clay.Stylesheet (Feature (..), MediaType (..), rule)
import qualified Skylighting as S

data Base16 = Base16
    { base00 :: Color
    , base01 :: Color
    , base02 :: Color
    , base03 :: Color
    , base04 :: Color
    , base05 :: Color
    , base06 :: Color
    , base07 :: Color
    , base08 :: Color
    , base09 :: Color
    , base0A :: Color
    , base0B :: Color
    , base0C :: Color
    , base0D :: Color
    , base0E :: Color
    , base0F :: Color
    }

base16DefaultDark =
    Base16
        { base00 = "#181818"
        , base01 = "#282828"
        , base02 = "#383838"
        , base03 = "#585858"
        , base04 = "#b8b8b8"
        , base05 = "#d8d8d8"
        , base06 = "#e8e8e8"
        , base07 = "#f8f8f8"
        , base08 = "#ab4642"
        , base09 = "#dc9656"
        , base0A = "#f7ca88"
        , base0B = "#a1b56c"
        , base0C = "#86c1b9"
        , base0D = "#7cafc2"
        , base0E = "#ba8baf"
        , base0F = "#a16946"
        }

base16DefaultLight =
    Base16
        { base00 = "#f8f8f8"
        , base01 = "#e8e8e8"
        , base02 = "#d8d8d8"
        , base03 = "#b8b8b8"
        , base04 = "#585858"
        , base05 = "#383838"
        , base06 = "#282828"
        , base07 = "#181818"
        , base08 = "#ab4642"
        , base09 = "#dc9656"
        , base0A = "#f7ca88"
        , base0B = "#a1b56c"
        , base0C = "#86c1b9"
        , base0D = "#7cafc2"
        , base0E = "#ba8baf"
        , base0F = "#a16946"
        }

code_ :: Css
code_ = do
    pre ? preStyle
    code ? codeStyle
    pre ? preTheme base16DefaultLight
    code ? codeTheme base16DefaultLight
    query screen [Feature "prefers-color-scheme" $ Just "dark"] $ do
        pre ? preTheme base16DefaultDark
        code ? codeTheme base16DefaultDark

preStyle :: Css
preStyle = do
    padding2 (em 0.5) (em 1)

preTheme :: Base16 -> Css
preTheme Base16{..} = do
    ".code" & backgroundColor base00

codeStyle :: Css
codeStyle = do
    fontFamily ["Source Code Pro"] []

codeTheme :: Base16 -> Css
codeTheme Base16{..} = do
    tokenTag ? do
        byClass (tokenClass S.AlertTok) & do
            backgroundColor base08
            fontWeight bold
            color "#000000"
        byClass (tokenClass S.AnnotationTok) & do
            color base04
        byClass (tokenClass S.AttributeTok) & do
            color base0D
        byClass (tokenClass S.BaseNTok) & do
            color base09
        byClass (tokenClass S.BuiltInTok) & do
            color base0D
        byClass (tokenClass S.CharTok) & do
            color base0C
        byClass (tokenClass S.CommentTok) & do
            fontStyle italic
            color base03
        byClass (tokenClass S.CommentVarTok) & do
            color base0C
        byClass (tokenClass S.ConstantTok) & do
            fontWeight bold
            color base09
        byClass (tokenClass S.ControlFlowTok) & do
            color base09
        byClass (tokenClass S.DataTypeTok) & do
            color base0A
        byClass (tokenClass S.DecValTok) & do
            color base09
        byClass (tokenClass S.DocumentationTok) & do
            color base08
        byClass (tokenClass S.ErrorTok) & do
            textDecoration underline
            color base08
        byClass (tokenClass S.ExtensionTok) & do
            fontWeight bold
            color base0D
        byClass (tokenClass S.FloatTok) & do
            color base09
        byClass (tokenClass S.FunctionTok) & do
            color base0D
        byClass (tokenClass S.ImportTok) & do
            color base0F
        byClass (tokenClass S.InformationTok) & do
            color base0C
        byClass (tokenClass S.KeywordTok) & do
            fontWeight bold
            color base0E
        byClass (tokenClass S.OperatorTok) & do
            color base05
        byClass (tokenClass S.OtherTok) & do
            color base0A
        byClass (tokenClass S.PreprocessorTok) & do
            color base0B
        byClass (tokenClass S.RegionMarkerTok) & do
            color base07
        byClass (tokenClass S.SpecialCharTok) & do
            color base0C
        byClass (tokenClass S.SpecialStringTok) & do
            color base0F
        byClass (tokenClass S.StringTok) & do
            color base0B
        byClass (tokenClass S.VariableTok) & do
            color base08
        byClass (tokenClass S.VerbatimStringTok) & do
            color base0B
        byClass (tokenClass S.WarningTok) & do
            color base0A
