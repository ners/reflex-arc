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

token :: (IsToken s, DomBuilder t m) => (s, Text) -> m ()
token (tokenType, txt) = elClass tokenTag (tokenClass tokenType) $ text txt

class IsToken t where
    tokenClass :: t -> Text

instance IsToken S.TokenType where
    tokenClass = tshow

instance IsToken GS.Token where
    tokenClass GS.CharTok = tokenClass S.CharTok
    tokenClass GS.CommentTok = tokenClass S.CommentTok
    tokenClass GS.ConstructorTok = tokenClass S.FunctionTok
    tokenClass GS.IntegerTok = tokenClass S.ConstantTok
    tokenClass GS.KeywordTok = tokenClass S.KeywordTok
    tokenClass GS.OperatorTok = tokenClass S.OperatorTok
    tokenClass GS.OtherTok = tokenClass S.OtherTok
    tokenClass GS.PragmaTok = tokenClass S.PreprocessorTok
    tokenClass GS.RationalTok = tokenClass S.FloatTok
    tokenClass GS.SpaceTok = ""
    tokenClass GS.StringTok = tokenClass S.StringTok
    tokenClass GS.SymbolTok = tokenClass S.OperatorTok
    tokenClass GS.VariableTok = tokenClass S.VariableTok
