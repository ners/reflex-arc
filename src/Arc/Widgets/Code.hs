module Arc.Widgets.Code (module Skylighting, CodeBlock (..), codeBlock) where

import Arc.Util
import Data.Bifunctor (first)
import Data.Default (Default)
import Data.List (intersperse)
import Data.List.Extra (split)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.SyntaxHighlighter qualified as GS
import Reflex.Dom hiding (current)
import Skylighting (SourceLine, Token, TokenType (..))
import Skylighting qualified as S

data CodeBlock = CodeBlock
    { codeBlockLanguage :: Maybe Text
    , codeBlockContent :: Text
    }

instance Default CodeBlock where
    def = CodeBlock Nothing ""

codeBlock :: DomBuilder t m => CodeBlock -> m ()
codeBlock CodeBlock{..} = elClass "pre" "code" $
    mapM_ line $
        case codeBlockLanguage of
            Just "Haskell" -> highlightHaskell codeBlockContent
            Just lang -> highlightLang lang codeBlockContent
            Nothing -> plainSource codeBlockContent

plainSource :: Text -> [SourceLine]
plainSource src = [[(NormalTok, l)] | l <- Text.lines src]

line :: DomBuilder t m => S.SourceLine -> m ()
line l = do
    elClass "span" "line-number" $ pure ()
    elClass "code" "line" $ mapM_ token l

token :: DomBuilder t m => Token -> m ()
token (NormalTok, txt) = text txt
token (tokenType, txt) = elClass "span" (className tokenType) $ text txt

highlightHaskell :: Text -> [SourceLine]
highlightHaskell code = case GS.tokenizeHaskell code of
    Just toks -> gsTokensToLines toks
    Nothing -> plainSource code

highlightLang :: Text -> Text -> [SourceLine]
highlightLang lang code = do
    let cfg =
            S.TokenizerConfig
                { syntaxMap = S.defaultSyntaxMap
                , traceOutput = False
                }
    case S.lookupSyntax lang (S.syntaxMap cfg) of
        Nothing -> plainSource code
        Just syntax -> case S.tokenize cfg syntax code of
            Left _ -> plainSource code
            Right toks -> toks

gsTokensToLines :: [(GS.Token, Text)] -> [SourceLine]
gsTokensToLines = split (\(_, tokenText) -> tokenText == "\n") . concatMap splitTokenLines . fmap (first gs2tok)

splitTokenLines :: Token -> [Token]
splitTokenLines (tokenType, tokenText) = (tokenType,) <$> intersperse "\n" (Text.splitOn "\n" tokenText)

gs2tok :: GS.Token -> TokenType
gs2tok GS.CharTok = CharTok
gs2tok GS.CommentTok = CommentTok
gs2tok GS.ConstructorTok = FunctionTok
gs2tok GS.IntegerTok = ConstantTok
gs2tok GS.KeywordTok = KeywordTok
gs2tok GS.OperatorTok = OperatorTok
gs2tok GS.OtherTok = OtherTok
gs2tok GS.PragmaTok = PreprocessorTok
gs2tok GS.RationalTok = FloatTok
gs2tok GS.SpaceTok = NormalTok
gs2tok GS.StringTok = StringTok
gs2tok GS.SymbolTok = OperatorTok
gs2tok GS.VariableTok = VariableTok

instance ClassName TokenType

deriving stock instance Bounded TokenType
