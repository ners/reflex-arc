module Arc.Util where

import Data.Maybe
import Data.Foldable

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Reflex
import Reflex.Dom
import Reflex.Dynamic

tshow :: Show a => a -> Text
tshow = Text.pack . show

mkAttrs :: Ord a => [(a, Text)] -> Map a Text
mkAttrs = Map.fromList
 
mkDynAttrs :: Reflex t => [(Text, Text)] -> Dynamic t (Map Text Text)
mkDynAttrs = constDyn . Map.fromList

div :: DomBuilder t m => m a -> m a
div = el "div"

findJust :: Foldable t => t (Maybe a) -> Maybe a
findJust ms = find isJust ms >>= id

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x