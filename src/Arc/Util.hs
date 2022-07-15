{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Util where

import Control.Monad (join)
import Data.Foldable
import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Default (Default)
import Data.Functor ((<&>))
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

a :: DomBuilder t m => m () -> m (Event t ())
a w = do
    (e, _) <- el' "a" w
    return $ domEvent Click e

update :: (DomBuilder t m, PostBuild t m) => (a -> m ()) -> Dynamic t a -> m ()
update f d = dyn_ $ f <$> d

class Clickable e where
    clickableTag :: Text
    clickableTag = "a"
    clickableContent :: DomBuilder t m => e -> m ()
    default clickableContent :: (Show e, DomBuilder t m) => e -> m ()
    clickableContent = text . tshow
    clickable :: DomBuilder t m => e -> m (Event t ())
    clickable e = do
        (e, _) <- el' (clickableTag @e) (clickableContent e)
        return $ domEvent Click e

class Eq e => Selectable e where
    selectableTag :: Text
    selectableTag = "li"
    selectableContent :: DomBuilder t m => e -> m (Event t ())
    default selectableContent :: (Clickable e, DomBuilder t m) => e -> m (Event t ())
    selectableContent = clickable
    selectable :: (DomBuilder t m, PostBuild t m) => e -> Dynamic t (Maybe e) -> m (Event t e)
    default selectable :: (Clickable e, DomBuilder t m, PostBuild t m) => e -> Dynamic t (Maybe e) -> m (Event t e)
    selectable e d = do
        let className = d <&> \f -> if Just e == f then "selected" else ""
        c <- elDynClass (selectableTag @e) className (clickable e)
        return $ e <$ c
