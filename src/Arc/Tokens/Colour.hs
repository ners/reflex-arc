module Arc.Tokens.Colour where

import Arc.Util (ishow)
import Clay hiding (s)
import Clay.Media (screen)
import Clay.Stylesheet (Feature (Feature))
import Control.Monad (forM_)

class ColourToken ct where
    foregroundColour :: ct -> Color
    backgroundColour :: ct -> Color
    backgroundColour _ = transparent

class ColourSchemeToken ct where
    foregroundColourScheme :: ct -> ColourScheme -> Color
    default foregroundColourScheme :: ct -> ColourScheme -> Color
    foregroundColourScheme _ _ = inherit
    backgroundColourScheme :: ct -> ColourScheme -> Color
    default backgroundColourScheme :: ct -> ColourScheme -> Color
    backgroundColourScheme _ _ = transparent

data ColourScheme = LightColourScheme | DarkColourScheme deriving stock (Eq)

instance Show ColourScheme where
    show LightColourScheme = "light"
    show DarkColourScheme = "dark"

withColourScheme :: (ColourScheme -> Css) -> Css
withColourScheme f = forM_ [LightColourScheme, DarkColourScheme] $ \s ->
    query screen [Feature "prefers-color-scheme" $ Just $ ishow s] (f s)

applyColourScheme :: ColourSchemeToken t => t -> Css
applyColourScheme t = withColourScheme $ \s -> do
    color $ foregroundColourScheme t s
    backgroundColor $ backgroundColourScheme t s

-- Colour values

red :: Color
red = rgb 222 53 11

blue :: Color
blue = rgb 0 82 204

grey :: Color
grey = rgb 9 30 66

white :: Color
white = rgb 255 255 255

green :: Color
green = rgb 0 102 68

orange :: Color
orange = rgb 255 171 0

black :: Color
black = rgb 23 43 77

data Base16 = Base16
    { base00 :: Color
    , base01 :: Color
    , base02 :: Color
    , base03 :: Color
    , base04 :: Color
    , base05 :: Color
    , base06 :: Color
    , base07 :: Color
    , base08 :: Color
    , base09 :: Color
    , base0A :: Color
    , base0B :: Color
    , base0C :: Color
    , base0D :: Color
    , base0E :: Color
    , base0F :: Color
    }

base16Default :: ColourScheme -> Base16
base16Default LightColourScheme = base16DefaultLight
base16Default DarkColourScheme = base16DefaultDark

base16DefaultDark :: Base16
base16DefaultDark =
    Base16
        { base00 = "#181818"
        , base01 = "#282828"
        , base02 = "#383838"
        , base03 = "#585858"
        , base04 = "#b8b8b8"
        , base05 = "#d8d8d8"
        , base06 = "#e8e8e8"
        , base07 = "#f8f8f8"
        , base08 = "#ab4642"
        , base09 = "#dc9656"
        , base0A = "#f7ca88"
        , base0B = "#a1b56c"
        , base0C = "#86c1b9"
        , base0D = "#7cafc2"
        , base0E = "#ba8baf"
        , base0F = "#a16946"
        }

base16DefaultLight :: Base16
base16DefaultLight =
    Base16
        { base00 = "#f8f8f8"
        , base01 = "#e8e8e8"
        , base02 = "#d8d8d8"
        , base03 = "#b8b8b8"
        , base04 = "#585858"
        , base05 = "#383838"
        , base06 = "#282828"
        , base07 = "#181818"
        , base08 = "#ab4642"
        , base09 = "#dc9656"
        , base0A = "#f7ca88"
        , base0B = "#a1b56c"
        , base0C = "#86c1b9"
        , base0D = "#7cafc2"
        , base0E = "#ba8baf"
        , base0F = "#a16946"
        }
