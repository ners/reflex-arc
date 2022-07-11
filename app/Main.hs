{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Arc.Main
import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Icon
import Arc.Widgets.OptionGroup
import Arc.Widgets.Text
import Control.Monad (join)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Reflex.Dom hiding (Checkbox, button)
import Reflex.Dom.MDI
import Prelude hiding (div)

data ColourGroup = Red | Green | Blue | Other Text

instance Enum ColourGroup where
    fromEnum Red = 1
    fromEnum Green = 2
    fromEnum Blue = 3
    fromEnum (Other _) = 4

instance OptionGroup ColourGroup where
    groupName = "colour"
    groupType = Radio
    optionLabel Red = "Red"
    optionLabel Green = "Green"
    optionLabel Blue = "Blue"
    optionLabel (Other _) = "Other"
    groupInputMultiEl = el "fieldset" $ do
        options <- do
            rgb <- mapM optionInputGroupEl [Red, Green, Blue]
            o <- otherEl
            return $ o : rgb
        return $ catMaybes <$> sequence options
      where
        otherEl ::
            forall t m.
            (DomBuilder t m, PostBuild t m) =>
            m (Dynamic t (Maybe ColourGroup))
        otherEl = inputGroup @ColourGroup $ do
            radio <- optionInputEl $ Other "" :: m (Dynamic t (Maybe ColourGroup))
            optionLabelEl $ Other ""
            colourText <- divClass "input-group" $ do
                el "span" $ text "Choose colour:"
                textBox $ def{textBoxSize = Inline}
            return $ zipDynWith (\c r -> Other c <$ r) colourText radio

data GenderGroup = Male | Female | Apache | Unisex deriving (Show, Bounded, Enum)

instance OptionGroup GenderGroup where
    groupName = "gender"
    groupType = Checkbox

main :: IO ()
main = arcMain $ do
    div $ do
        el "span" $ text "Hello world!!"
        icon $ def{iconImage = mdiTrophy}
    div $ radioButtonGroup @ColourGroup
    div $ checkboxGroup @GenderGroup
    div $ do
        _ <-
            button $
                def
                    { buttonContent = "Danger"
                    , buttonVariant = Danger
                    , buttonLeftIcon = Just $ def{iconImage = mdiTrashCan}
                    }
        _ <-
            button $
                def
                    { buttonContent = "Warning"
                    , buttonVariant = Warning
                    , buttonLeftIcon = Just $ def{iconImage = mdiAlertRhombus}
                    }
        _ <-
            button $
                def
                    { buttonContent = "Primary"
                    , buttonVariant = Primary
                    , buttonLeftIcon = Just $ def{iconImage = mdiStar}
                    }
        _ <-
            button $
                def
                    { buttonContent = "Default"
                    , buttonVariant = Default
                    , buttonLeftIcon = Just $ def{iconImage = mdiAccountCircle}
                    }
        blank
    div $ do
        _ <-
            button $
                def
                    { buttonContent = "Danger"
                    , buttonVariant = Danger
                    }
        _ <-
            button $
                def
                    { buttonContent = "Warning"
                    , buttonVariant = Warning
                    }
        _ <-
            button $
                def
                    { buttonContent = "Primary"
                    , buttonVariant = Primary
                    }
        _ <-
            button $
                def
                    { buttonContent = "Default"
                    , buttonVariant = Default
                    }
        blank
    div $ do
        _ <-
            button $
                def
                    { buttonContent = "Danger"
                    , buttonVariant = Danger
                    , buttonDisabled = True
                    }
        _ <-
            button $
                def
                    { buttonContent = "Warning"
                    , buttonVariant = Warning
                    , buttonDisabled = True
                    }
        _ <-
            button $
                def
                    { buttonContent = "Primary"
                    , buttonVariant = Primary
                    , buttonDisabled = True
                    }
        _ <-
            button $
                def
                    { buttonContent = "Default"
                    , buttonVariant = Default
                    , buttonDisabled = True
                    }
        blank
