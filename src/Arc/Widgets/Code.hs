module Arc.Widgets.Code where

import Arc.Util
import Control.Monad (forM_)
import Data.Default (Default)
import Data.String (IsString)
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
codeBlock CodeBlock{..} = elClass "pre" "code" $
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

tokenTag :: IsString s => s
tokenTag = "span"

token :: (ClassName s, DomBuilder t m) => (s, Text) -> m ()
token (tokenType, txt) = elClass tokenTag (className tokenType) $ text txt

instance ClassName S.TokenType

instance ClassName GS.Token where
    className GS.CharTok = className S.CharTok
    className GS.CommentTok = className S.CommentTok
    className GS.ConstructorTok = className S.FunctionTok
    className GS.IntegerTok = className S.ConstantTok
    className GS.KeywordTok = className S.KeywordTok
    className GS.OperatorTok = className S.OperatorTok
    className GS.OtherTok = className S.OtherTok
    className GS.PragmaTok = className S.PreprocessorTok
    className GS.RationalTok = className S.FloatTok
    className GS.SpaceTok = ""
    className GS.StringTok = className S.StringTok
    className GS.SymbolTok = className S.OperatorTok
    className GS.VariableTok = className S.VariableTok
