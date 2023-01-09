{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Util where

import Control.Monad (join)
import Data.Foldable
import Data.Maybe

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Data.Bool (bool)
import Data.Char (isAlphaNum)
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Reflex
import Reflex.Dom hiding (AttributeMap)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

tshow :: Show a => a -> Text
tshow = ishow

div :: forall a t m. DomBuilder t m => m a -> m a
div = el "div"

elId :: forall a t m. DomBuilder t m => Text -> Text -> m a -> m a
elId name id' = elAttr name $ mkAttrs [Just ("id", id')]

elAttrNS :: forall a t m. DomBuilder t m => Maybe Text -> Text -> Map Text Text -> m a -> m a
elAttrNS mns elementTag attrs child = snd <$> elAttrNS' mns elementTag attrs child

elAttrNS' :: forall a t m. DomBuilder t m => Maybe Text -> Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elAttrNS' mns elementTag attrs =
    element elementTag $
        def
            & elementConfig_namespace .~ mns
            & initialAttributes .~ Map.mapKeysMonotonic (AttributeName Nothing) attrs

labelFor :: forall a t m. DomBuilder t m => Maybe Text -> m a -> m a
labelFor id' = elAttr "label" $ mkAttrs [("for",) <$> id']

findJust :: forall a t. Foldable t => t (Maybe a) -> Maybe a
findJust ms = join $ find isJust ms

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

mkAttrs :: forall k v. Ord k => [Maybe (k, v)] -> Map k v
mkAttrs = Map.fromList . catMaybes

mkDynAttrs :: forall k v t. Ord k => Reflex t => [Maybe (k, v)] -> Dynamic t (Map k v)
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

update
    :: forall a b t m
     . DomBuilder t m
    => PostBuild t m
    => Dynamic t a
    -> (a -> m b)
    -> m (Event t b)
update d f = dyn $ f <$> d

class ToElement r where
    toElement :: DomBuilder t m => r -> m ()
    default toElement :: Show r => DomBuilder t m => r -> m ()
    toElement r = text $ tshow r

instance ToElement () where
    toElement () = blank

instance ToElement Char where
    toElement = text . Text.singleton

instance ToElement Text where
    toElement = text

class Clickable e where
    clickableTag :: Text
    clickableTag = "a"
    clickableClass :: Text
    clickableClass = ""
    clickableContent :: DomBuilder t m => e -> m ()
    default clickableContent :: (ToElement e, DomBuilder t m) => e -> m ()
    clickableContent = toElement @e
    clickable :: DomBuilder t m => e -> m (Event t ())
    clickable e = do
        (e', _) <- elClass' (clickableTag @e) (clickableClass @e) (clickableContent e)
        return $ domEvent Click e'

class (Eq e) => Selectable e where
    selectableTag :: Text
    selectableTag = "li"
    selectableClass :: Text
    selectableClass = ""
    selectableContent :: (DomBuilder t m, PostBuild t m) => e -> m (Event t ())
    default selectableContent :: (Clickable e, DomBuilder t m) => e -> m (Event t ())
    selectableContent = clickable
    selectable :: (DomBuilder t m, PostBuild t m) => e -> Dynamic t (Maybe e) -> m (Event t e)
    selectable e d = do
        let className' = do
                selected <- bool "" "selected" . (Just e ==) <$> d
                pure $ Text.unwords $ filter (not . Text.null) [selectableClass @e, selected]
        c <- elDynClass (selectableTag @e) className' (selectableContent e)
        return $ e <$ c

class ClassName c where
    className :: IsString s => c -> s
    default className :: (Show c, IsString s) => c -> s
    className = fromString . show

class ClassName c => BaseClassName c where
    baseClassName :: IsString s => s
    fullClassName :: IsString s => c -> [s]
    fullClassName c = [baseClassName @c, className c]

fullClassString :: IsString s => BaseClassName c => c -> s
fullClassString = fromString . unwords . fullClassName

fullClassAttr :: IsString s => BaseClassName c => c -> Maybe (s, s)
fullClassAttr c = Just ("class", fullClassString c)

nonEmpty :: Text -> Bool
nonEmpty = any isAlphaNum . Text.unpack
