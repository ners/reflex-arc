module Arc.Clay.Buttons where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Arc.Widgets.Button (ButtonVariant (..))
import Arc.Widgets.Icon
import Clay hiding (a, black, blue, grey, orange, red, white)
import Control.Monad (forM_)

buttons :: Css
buttons = do
    button ? buttonStyle
    ".buttons" ? do
        button ? float floatLeft
        after & do
            content $ Content "''"
            display block
            clear clearLeft

instance ColourSchemeToken ButtonVariant where
    backgroundColourScheme DangerButton _ = setA 0.8 red
    backgroundColourScheme DefaultButton _ = setA 0.1 grey
    backgroundColourScheme GhostButton _ = transparent
    backgroundColourScheme PrimaryButton _ = setA 0.8 blue
    backgroundColourScheme WarningButton _ = setA 0.8 orange
    foregroundColourScheme DangerButton = const white
    foregroundColourScheme DefaultButton = foregroundColourScheme BaseColour
    foregroundColourScheme GhostButton = foregroundColourScheme BaseColour
    foregroundColourScheme PrimaryButton = const white
    foregroundColourScheme WarningButton = const white

buttonStyle :: Css
buttonStyle = do
    borderWidth $ em 0
    borderStyle none
    borderRadiusAll $ em 0.2
    display inlineBlock
    height $ em 2.5
    lineHeight $ em 2.5
    marginAll $ em 0.2
    padding2 (em 0) (em 1)
    fontWeight $ weight 400
    cursor pointer
    position relative
    self |> (star <> baseClass_ @Icon) ? do
        margin2 (em 0) (em 0.3)
    forM_ [minBound .. maxBound] $ \(bv :: ButtonVariant) ->
        class_ bv & do
            applyColourScheme bv
            hover
                & withColourScheme
                    ( \cs -> do
                        let bg = backgroundColourScheme bv cs
                        backgroundColor $ modifyA (+ 0.2) bg
                    )
            active
                & withColourScheme
                    ( \cs -> do
                        let bg = backgroundColourScheme bv cs
                        backgroundColor $ modifyA (+ 0.2) bg
                    )
    transition "background-color" (ms 100) easeInOut (sec 0)
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
