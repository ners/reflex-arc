{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Arc.Widgets.OptionGroup where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Prelude hiding (div)

import Arc.Util
import Arc.Widgets.Checkbox
import Reflex.Dom hiding (Checkbox, checkbox)

type OptionGroupType = CheckboxType

class OptionGroup g where
    groupName :: Text
    groupLegend :: Maybe Text
    groupLegend = Nothing
    groupType :: OptionGroupType
    inputGroup :: DomBuilder t m => m a -> m a
    inputGroup = divClass $ "input-group " <> showType (groupType @g)
    optionId :: g -> Text
    default optionId :: (Enum g) => g -> Text
    optionId g = groupName @g <> "-" <> tshow (fromEnum g)
    optionLabel :: g -> Text
    default optionLabel :: (Show g) => g -> Text
    optionLabel = tshow
    optionLabelEl :: DomBuilder t m => g -> m ()
    optionLabelEl g = elAttr "label" (mkAttrs [Just ("for", optionId g)]) $ text $ optionLabel g
    optionInputEl :: (DomBuilder t m) => g -> m (Dynamic t (Maybe g))
    optionInputEl g = do
        i <-
            checkbox $
                def
                    { checkboxName = Just $ groupName @g
                    , checkboxId = Just $ optionId g
                    , checkboxType = groupType @g
                    }
        return $ do
            checked <- i
            if checked
                then return $ Just g
                else return Nothing
    optionInputGroupEl :: DomBuilder t m => g -> m (Dynamic t (Maybe g))
    optionInputGroupEl g = inputGroup @g $ do
        i <- optionInputEl g
        optionLabelEl g
        return i
    groupInputMultiEl :: (DomBuilder t m, PostBuild t m) => m (Dynamic t [g])
    default groupInputMultiEl :: (Bounded g, Enum g, DomBuilder t m) => m (Dynamic t [g])
    groupInputMultiEl = el "fieldset" $ do
        maybe blank (el "legend" . text) $ groupLegend @g
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
