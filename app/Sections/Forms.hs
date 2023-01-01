{-# LANGUAGE LambdaCase #-}

module Sections.Forms where

import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Checkbox
import Arc.Widgets.Form
import Arc.Widgets.OptionGroup
import Arc.Widgets.Text
import Arc.Widgets.Textarea
import Control.Monad (void)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Reflex.Dom hiding (button, checkbox, textInput)

newtype Username = Username Text

newtype Password = Password Text

newtype Email = Email Text

data Gender = MaleGender | FemaleGender | OtherGender Text
    deriving stock (Show)

newtype Bio = Bio Text

newtype Newsletter = Newsletter Bool

newtype Eula = Eula Bool

instance OptionGroup Gender where
    groupLegend = Just "Gender"
    groupName = "gender"
    groupType = RadioType
    optionId MaleGender = "MaleGender"
    optionId FemaleGender = "FemaleGender"
    optionId (OtherGender _) = "OtherGender"
    optionLabel MaleGender = "Male"
    optionLabel FemaleGender = "Female"
    optionLabel (OtherGender _) = "Other"
    groupInputMultiEl = el "fieldset" $ do
        maybe blank (el "legend" . text) $ groupLegend @Gender
        options <- do
            mf <- mapM optionInputGroupEl [MaleGender, FemaleGender]
            o <- otherEl
            return $ o : mf
        return $ catMaybes <$> sequence options
      where
        otherEl ::
            forall t m.
            (DomBuilder t m, PostBuild t m) =>
            m (Dynamic t (Maybe Gender))
        otherEl = inputGroup @Gender $ do
            radio <- optionInputEl $ OtherGender "" :: m (Dynamic t (Maybe Gender))
            optionLabelEl $ OtherGender ""
            genderText <- update radio $ \case
                Just _ ->
                    divClass "input-group" $
                        textInput $
                            def
                                { textInputId = Just "gender-other"
                                , textInputLabel = Just "Please specify:"
                                , textInputSize = TextInputInline
                                }
                _ -> return $ constDyn ""
            return $ return $ Just $ OtherGender ""

-- return $ zipDynWith (\g r -> OtherGender g <$ r) genderText radio

instance FormField Username where
    fieldRequired = True
    fieldInputElement = Username <<$>> textInput c
      where
        c =
            def
                { textInputId = Just "login-username"
                , textInputLabel = Just "Username"
                }

instance FormField Email where
    fieldRequired = True
    fieldInputElement = Email <<$>> emailInput c
      where
        c =
            def
                { textInputId = Just "login-email"
                , textInputLabel = Just "Email"
                }

instance FormField Password where
    fieldRequired = True
    fieldInputElement = Password <<$>> passwordInput c
      where
        c =
            def
                { textInputId = Just "login-password"
                , textInputLabel = Just "Password"
                }

instance FormField Bio where
    fieldRequired = False
    fieldInputElement = Bio <<$>> textarea c
      where
        c =
            def
                { textareaId = Just "login-bio"
                , textareaLabel = Just "Your life story"
                }

instance FormField Newsletter where
    fieldRequired = False
    fieldInputElement = Newsletter <<$>> checkbox c
      where
        c =
            def
                { checkboxId = Just "login-newsletter"
                , checkboxLabel = Just "Subscribe to newsletter"
                }

instance FormField Eula where
    fieldRequired = True
    fieldInputElement = Eula <<$>> checkbox c
      where
        c =
            def
                { checkboxId = Just "login-eula"
                , checkboxLabel = Just "I have read and agree to the terms and conditions"
                }

data SignupForm = SignupForm
    { username :: Username
    , email :: Email
    , gender :: Gender
    , password :: Password
    , bio :: Bio
    , newsletter :: Newsletter
    , eula :: Eula
    }

instance Form SignupForm where
    formTitle = Just "Sign up"
    formFields = do
        u <- formField @Username
        e <- formField @Email
        g <- groupInputSingleEl @Gender
        p <- formField @Password
        b <- formField @Bio
        n <- checkboxFormField @Newsletter
        l <- checkboxFormField @Eula
        divClass "buttons" $ do
            button $ def{buttonContent = "Cancel", buttonVariant = GhostButton}
            button $ def{buttonContent = "Submit", buttonVariant = PrimaryButton}
            pure ()
        return $
            SignupForm
                <$> u
                <*> e
                <*> (fromMaybe (OtherGender "") <$> g)
                <*> p
                <*> b
                <*> n
                <*> l

formsSection :: (DomBuilder t m, PostBuild t m) => m ()
formsSection = do
    void $ form @SignupForm
