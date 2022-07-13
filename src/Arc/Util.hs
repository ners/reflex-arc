module Arc.Util where

import Control.Monad (join)
import Data.Foldable
import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Default (Default)
import Data.String (IsString)
import Reflex
import Reflex.Dom
import Reflex.Dynamic

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

tshow :: Show a => a -> Text
tshow = Text.pack . show

div :: DomBuilder t m => m a -> m a
div = el "div"

elId :: DomBuilder t m => Text -> Text -> m a -> m a
elId name id' = elAttr name $ mkAttrs [Just ("id", id')]

labelFor :: DomBuilder t m => Maybe Text -> m a -> m a
labelFor id' = elAttr "label" $ mkAttrs [("for",) <$> id']

findJust :: Foldable t => t (Maybe a) -> Maybe a
findJust ms = join $ find isJust ms

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

mkAttrs :: Ord a => [Maybe (a, Text)] -> Map a Text
mkAttrs = Map.fromList . catMaybes

mkDynAttrs :: Reflex t => [Maybe (Text, Text)] -> Dynamic t (Map Text Text)
mkDynAttrs = constDyn . mkAttrs

toMaybe :: Bool -> a -> Maybe a
toMaybe False = const Nothing
toMaybe True = Just

maybeDisabled :: IsString s => Bool -> Maybe (s, Text)
maybeDisabled b = toMaybe b ("disabled", "")

class WithId f where
    idAttr :: (f -> Maybe Text)
    defId :: Text -> f

class ShowType t where
    showType :: t -> Text
