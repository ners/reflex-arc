module Arc.Clay.Buttons where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Arc.Widgets.Button (ButtonVariant (..))
import Arc.Widgets.Icon
import Clay hiding (a, black, blue, grey, orange, red, white)
import Control.Monad (forM_)

buttons :: Css
buttons = button ? buttonStyle

instance ColourSchemeToken ButtonVariant where
    backgroundColourScheme PrimaryButton _ = setA 0.8 blue
    backgroundColourScheme WarningButton _ = setA 0.8 orange
    backgroundColourScheme DefaultButton LightColourScheme = setA 0.1 black
    backgroundColourScheme DefaultButton DarkColourScheme = setA 0.05 white
    backgroundColourScheme DangerButton _ = setA 0.8 red
    backgroundColourScheme GhostButton _ = transparent
    foregroundColourScheme PrimaryButton = const white
    foregroundColourScheme DangerButton = const white
    foregroundColourScheme WarningButton = const black
    foregroundColourScheme _ = base05 . base16Default

buttonStyle :: Css
buttonStyle = do
    borderWidth $ em 0
    borderStyle none
    borderRadiusAll $ em 0.2
    display inlineBlock
    height $ em 2
    lineHeight $ em 2
    marginAll $ em 0.2
    padding2 (em 0) (em 1)
    fontWeight $ weight 500
    cursor pointer
    position relative
    self |> (star <> baseClass_ @Icon) ? do
        margin2 (em 0) (em 0.3)
    forM_ [minBound .. maxBound] $ \(bv :: ButtonVariant) ->
        class_ bv & do
            applyColourScheme bv
            hover & withColourScheme (\cs -> do
                let bg = backgroundColourScheme bv cs
                backgroundColor $ modifyA (+ 0.2) bg
                )
    class_ GhostButton & hover & textDecoration underline
    disabled & do
        opacity 0.5
        pointerEvents none
    where
        getA (Rgba _ _ _ a) = a
        getA (Hsla _ _ _ a) = a
        getA _ = 1
        modifyA f c
            | c == transparent = transparent
            | otherwise = setA (f $ getA c) c
