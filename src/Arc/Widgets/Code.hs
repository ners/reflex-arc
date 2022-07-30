module Arc.Widgets.Code where

import Arc.Util
import Data.Default (Default)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.SyntaxHighlighter
import Reflex.Dom

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
            Nothing -> text codeBlockContent

highlightHaskell :: DomBuilder t m => Text -> m ()
highlightHaskell hs = case tokenizeHaskell hs of
    Just toks -> mapM_ tokenToHtml toks
    Nothing -> text hs

tokenToHtml :: DomBuilder t m => (Token, Text) -> m ()
tokenToHtml (tokenType, hs) = wrap hs
  where
    rawClass = tokenClass tokenType
    wrap = if Text.null rawClass then text else elClass "span" rawClass . text
    tokenClass KeywordTok = "kw"
    tokenClass PragmaTok = "pr"
    tokenClass SymbolTok = "sy"
    tokenClass VariableTok = "va"
    tokenClass ConstructorTok = "cr"
    tokenClass OperatorTok = "op"
    tokenClass CharTok = "ch"
    tokenClass StringTok = "st"
    tokenClass IntegerTok = "it"
    tokenClass RationalTok = "ra"
    tokenClass CommentTok = "co"
    tokenClass OtherTok = "ot"
    tokenClass _ = ""