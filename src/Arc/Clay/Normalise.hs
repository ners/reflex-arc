module Arc.Clay.Normalise where

import Arc.Clay.Util
import Clay
import Clay.Stylesheet

normalise :: Css
normalise = do
    normaliseDocument
    normaliseSections
    normaliseGrouping
    normaliseText
    normaliseEmbedded
    normaliseForms
    normaliseInteractive
    normaliseMisc

normaliseDocument :: Css
normaliseDocument = do
    html ? do
        -- Correct the line height in all browsers.
        lineHeight (unitless 1.15)
        -- Prevent adjustments of font size after orientation changes in iOS.
        key "-webkit-text-size-adjust" (pct 100)
    -- Correct body height in Safari.
    body ? minHeight (vh 100)

normaliseSections :: Css
normaliseSections = do
    -- Remove the margin in all browsers.
    body ? marginAll nil

    -- Render the `main` element consistently in IE.
    main_ ? display block

    -- Correct the font size and margin on `h1` elements within `section` and
    -- `article` contexts in Chrome, Firefox, and Safari.
    h1 ? do
        fontSize (em 2)
        margin2 (em 0.67) nil

normaliseGrouping :: Css
normaliseGrouping = do
    hr ? do
        -- Add the correct box sizing in Firefox.
        boxSizing contentBox
        height nil
        -- Show the overflow in Edge and IE.
        overflow visible

    pre ? do
        -- Correct the inheritance and scaling of font size in all browsers.
        fontFamily [] [monospace]
        -- Correct the odd `em` font sizing in all browsers.
        fontSize (em 1)

normaliseText :: Css
normaliseText = do
    -- Remove the gray background on active links in IE 10.
    a ? backgroundColor transparent

    abbr ? "[title]" & do
        -- Remove the bottom border in Chrome 57-
        key "borderBottom" $ Value "none"
        -- Add the correct text decoration in Chrome, Edge, IE, Opera, and Safari.
        textDecoration underline
        key "textDecoration" $ Value "underline dotted"

    -- Add the correct font weight in Chrome, Edge, and Safari.
    b <> strong ? fontWeight bolder

    code <> kbd <> samp ? do
        -- Correct the inheritance and scaling of font size in all browsers.
        fontFamily [] [monospace]
        -- Correct the odd `em` font sizing in all browsers.
        fontSize (em 1)

    -- Add the correct font size in all browsers.
    small ? fontSize (pct 80)

    -- Prevent `sub` and `sup` elements from affecting the line height in
    -- all browsers.
    sub <> sup ? do
        fontSize (pct 75)
        lineHeight nil
        position relative
        verticalAlign vAlignBaseline
    sub ? bottom (em $ -0.25)
    sup ? top (em $ -0.5)

normaliseEmbedded :: Css
normaliseEmbedded = do
    -- Remove the border on images inside links in IE 10.
    img ? do
        borderStyle none

normaliseForms :: Css
normaliseForms = do
    -- 1. Change the font styles in all browsers.
    -- 2. Remove the margin in Firefox and Safari.

    button <> input <> optgroup <> select <> textarea ? do
        fontFamily [] [inherit] -- 2
        fontSize (pct 100) -- 1
        lineHeight (unitless 1.15) -- 1
        marginAll nil -- 2

    -- Show the overflow in IE and Edge.
    button <> input ? overflow visible

    -- Remove the inheritance of text transform in Edge, Firefox, and IE.
    button <> select ? textTransform none

    -- Correct the inability to style clickable types in iOS and Safari.
    let buttonSelectors = button <> "[type=button]" <> "[type=reset]" <> "[type=submit]"

    buttonSelectors ? appearance (Value "button")

    -- Remove the inner border and padding in Firefox.
    buttonSelectors ? "::-moz-focus-inner" & do
        borderStyle none
        paddingAll nil

    -- Restore the focus styles unset by the previous rule.
    buttonSelectors ? ":-moz-focusring" & key "outline" (Value "1 px dotted buttonText")

    -- Correct the padding in Firefox.
    fieldset ? padding (em 0.35) (em 0.75) (em 0.625) (em 0.75)

    -- 1. Correct the text wrapping in Edge and IE.
    -- 2. Correct the color inheritance from `fieldset` elements in IE.
    -- 3. Remove the padding so developers are not caught out when they zero out
    --    `fieldset` elements in all browsers.
    legend ? do
        boxSizing borderBox
        color inherit -- 2
        display tableDisplay -- 1
        maxWidth (pct 100) -- 1
        paddingAll nil -- 3
        whiteSpace normal -- 1

    -- Add the correct vertical alignment in Chrome, Firefox, and Opera.
    progress ? verticalAlign vAlignBaseline

    -- Remove the default vertical scrollbar in IE 10+.
    textarea ? overflow auto

    -- 1. Add the correct box sizing in IE 10.
    -- 2. Remove the padding in IE 10.
    "[type=checkbox]" <> "[type=radio]" ? do
        boxSizing borderBox -- 1
        paddingAll nil -- 2

    -- Correct the cursor style of increment and decrement buttons in Chrome.
    "[type=number]" ? ":-webkit-inner-spin-button" & height auto
    "[type=number]" ? ":-webkit-outer-spin-button" & height auto

    -- 1. Correct the odd appearance in Chrome and Safari.
    -- 2. Correct the outline style in Safari.
    "[type=search]" ? do
        appearance $ Value "textfield" -- 1
        outlineOffset (px $ -2) -- 2

    -- Remove the inner padding in Chrome and Safari on macOS.
    "[type=search]" ? ":-webkit-search-decoration" & appearance noneValue

    -- 1. Correct the inability to style clickable types in iOS and Safari.
    -- 2. Change font properties to `inherit` in Safari.
    "::-webkit-file-upload-button" ? do
        appearance $ Value "button" -- 1
        font inheritFont -- 2

normaliseInteractive :: Css
normaliseInteractive = do
    -- Add the correct display in Edge, IE 10+, and Firefox.
    details ? display block

    -- Add the correct display in all browsers.
    summary ? display listItem

normaliseMisc :: Css
normaliseMisc = do
    -- Add the correct display in IE 10+.
    template ? display none

    -- Add the correct display in IE 10.
    "[hidden]" ? display none
