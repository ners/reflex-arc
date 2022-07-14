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
import Reflex.Dom hiding (AttributeMap)
import Reflex.Dynamic

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

tshow :: Show a => a -> Text
tshow = Text.pack . show

div :: DomBuilder t m => m a -> m a
div = el "div"

elId :: DomBuilder t m => Text -> Text -> m a -> m a
elId name id' = elAttr name $ mkAttrs [Just ("id", id')]

elAttrNS :: forall t m a. DomBuilder t m => Maybe Text -> Text -> Map Text Text -> m a -> m a
elAttrNS mns elementTag attrs child = snd <$> elAttrNS' mns elementTag attrs child

elAttrNS' :: forall t m a. DomBuilder t m => Maybe Text -> Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elAttrNS' mns elementTag attrs =
    element elementTag $
        def
            & elementConfig_namespace .~ mns
            & initialAttributes .~ Map.mapKeysMonotonic (AttributeName Nothing) attrs

labelFor :: DomBuilder t m => Maybe Text -> m a -> m a
labelFor id' = elAttr "label" $ mkAttrs [("for",) <$> id']

findJust :: Foldable t => t (Maybe a) -> Maybe a
findJust ms = join $ find isJust ms

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

mkAttrs :: (Ord k, IsString k, IsString v) => [Maybe (k, v)] -> Map k v
mkAttrs = Map.fromList . catMaybes

mkDynAttrs :: (Ord k, IsString k, IsString v) => Reflex t => [Maybe (k, v)] -> Dynamic t (Map k v)
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
