{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Arc.Widgets.OptionGroup where

import Prelude hiding (div)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Maybe (catMaybes)

import Arc.Util
import Reflex.Dom hiding (Checkbox)

data OptionGroupType = Checkbox | Radio

instance Show OptionGroupType where
    show Checkbox = "checkbox"
    show Radio = "radio"

class OptionGroup g where
    groupName :: Text
    groupType :: OptionGroupType
    inputGroup :: DomBuilder t m => m a -> m a
    inputGroup = divClass $ "input-group " <> (tshow $ groupType @g)
    optionId :: g -> Text
    default optionId :: (Enum g) => g -> Text
    optionId g = groupName @g <> "-" <> (tshow $ fromEnum g)
    optionValue :: g -> Text
    default optionValue :: (Enum g) => g -> Text
    optionValue = tshow . fromEnum
    optionLabel :: g -> Text
    default optionLabel :: (Show g) => g -> Text
    optionLabel = tshow
    optionLabelEl :: DomBuilder t m => g -> m ()
    optionLabelEl g = elAttr "label" (mkAttrs [("for", optionId g)]) $ text $ optionLabel g
    optionInputEl :: (DomBuilder t m, PostBuild t m) => g -> m (Dynamic t (Maybe g))
    optionInputEl g = do
        i <-
            inputElement $
                def
                    & inputElementConfig_elementConfig
                        . elementConfig_initialAttributes
                        .~ mkAttrs [("type", tshow (groupType @g)), ("name", groupName @g), ("id", optionId g), ("value", optionValue g)]
        return . fmap (\c -> if c then Just g else Nothing) $ _inputElement_checked i
    optionInputGroupEl :: (DomBuilder t m, PostBuild t m) => g -> m (Dynamic t (Maybe g))
    optionInputGroupEl g = inputGroup @g $ do
        i <- optionInputEl g
        optionLabelEl g
        return i
    groupInputMultiEl :: (DomBuilder t m, PostBuild t m) => m (Dynamic t [g])
    default groupInputMultiEl :: (DomBuilder t m, PostBuild t m, Bounded g, Enum g) => m (Dynamic t [g])
    groupInputMultiEl = el "fieldset" $ do
        inputs :: [Dynamic t (Maybe g)] <- mapM optionInputGroupEl [minBound .. maxBound]
        let checked :: Dynamic t [g] = catMaybes <$> sequence inputs
        return checked
    groupInputSingleEl :: (DomBuilder t m, PostBuild t m) => m (Dynamic t (Maybe g))
    groupInputSingleEl = do
        checked :: Dynamic t [g] <- groupInputMultiEl
        return $ headMaybe <$> checked

radioButtonGroup :: forall g t m. (OptionGroup g, DomBuilder t m, PostBuild t m) => m (Dynamic t (Maybe g))
radioButtonGroup = groupInputSingleEl @g

checkboxGroup :: forall g t m. (OptionGroup g, DomBuilder t m, PostBuild t m) => m (Dynamic t [g])
checkboxGroup = groupInputMultiEl @g
