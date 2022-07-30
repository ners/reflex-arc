module Arc.Widgets.Code where

import Arc.Util
import Control.Monad (forM_)
import Data.Default (Default)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHC.SyntaxHighlighter as GS
import Reflex.Dom
import qualified Skylighting as S

data CodeBlock = CodeBlock
    { codeBlockLanguage :: Maybe Text
    , codeBlockContent :: Text
    }

instance Default CodeBlock where
    def = CodeBlock Nothing ""

codeBlock :: DomBuilder t m => CodeBlock -> m ()
codeBlock CodeBlock{..} = el "pre" $
    el "code" $ do
        case codeBlockLanguage of
            Just "Haskell" -> highlightHaskell codeBlockContent
            Just lang -> highlightLang lang codeBlockContent
            Nothing -> text codeBlockContent

highlightLang :: DomBuilder t m => Text -> Text -> m ()
highlightLang "Haskell" code = highlightHaskell code
highlightLang lang code = do
    let cfg =
            S.TokenizerConfig
                { syntaxMap = S.defaultSyntaxMap
                , traceOutput = False
                }
    case S.lookupSyntax lang (S.syntaxMap cfg) of
        Nothing -> text code
        Just syntax -> case S.tokenize cfg syntax code of
            Left _ -> text code
            Right toks -> forM_ toks $ \t -> mapM_ token t >> text "\n"

highlightHaskell :: DomBuilder t m => Text -> m ()
highlightHaskell hs = case GS.tokenizeHaskell hs of
    Just toks -> mapM_ token toks
    Nothing -> text hs

token :: (IsToken s, DomBuilder t m) => (s, Text) -> m ()
token (tokenType, txt) = elClass "span" (tokenClass tokenType) $ text txt

class IsToken t where
    tokenClass :: t -> Text

instance IsToken GS.Token where
    tokenClass GS.CharTok = "ch"
    tokenClass GS.CommentTok = "co"
    tokenClass GS.ConstructorTok = "cr"
    tokenClass GS.IntegerTok = "it"
    tokenClass GS.KeywordTok = "kw"
    tokenClass GS.OperatorTok = "op"
    tokenClass GS.OtherTok = "ot"
    tokenClass GS.PragmaTok = "pr"
    tokenClass GS.RationalTok = "ra"
    tokenClass GS.SpaceTok = ""
    tokenClass GS.StringTok = "st"
    tokenClass GS.SymbolTok = "sy"
    tokenClass GS.VariableTok = "va"

instance IsToken S.TokenType where
    tokenClass S.KeywordTok = "kw"
    tokenClass S.DataTypeTok = "dt"
    tokenClass S.DecValTok = "dv"
    tokenClass S.BaseNTok = "bn"
    tokenClass S.FloatTok = "fl"
    tokenClass S.CharTok = "ch"
    tokenClass S.StringTok = "st"
    tokenClass S.CommentTok = "co"
    tokenClass S.OtherTok = "ot"
    tokenClass S.AlertTok = "al"
    tokenClass S.FunctionTok = "fu"
    tokenClass S.RegionMarkerTok = "re"
    tokenClass S.ErrorTok = "er"
    tokenClass S.ConstantTok = "cn"
    tokenClass S.SpecialCharTok = "sc"
    tokenClass S.VerbatimStringTok = "vs"
    tokenClass S.SpecialStringTok = "ss"
    tokenClass S.ImportTok = "im"
    tokenClass S.DocumentationTok = "do"
    tokenClass S.AnnotationTok = "an"
    tokenClass S.CommentVarTok = "cv"
    tokenClass S.VariableTok = "va"
    tokenClass S.ControlFlowTok = "cf"
    tokenClass S.OperatorTok = "op"
    tokenClass S.BuiltInTok = "bu"
    tokenClass S.ExtensionTok = "ex"
    tokenClass S.PreprocessorTok = "pp"
    tokenClass S.AttributeTok = "at"
    tokenClass S.InformationTok = "in"
    tokenClass S.WarningTok = "wa"
    tokenClass _ = ""
