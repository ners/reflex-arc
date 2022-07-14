module Arc.Clay.Fonts where

import Arc.Clay.Util
import Clay
import Clay.Stylesheet (key)

fonts :: Css
fonts = do
    -- face
    --     "Source Sans"
    --     [ (normal, woff2 $(embedFile "./src/Arc/Typography/woff2/SourceSans3VF-Roman.otf.woff2"))
    --     , (italic, woff2 $(embedFile "./src/Arc/Typography/woff2/SourceSans3VF-Italic.otf.woff2"))
    --     ]
    -- face
    --     "Source Serif"
    --     [ (normal, woff2 $(embedFile "./src/Arc/Typography/woff2/SourceSerif4Variable-Roman.otf.woff2"))
    --     , (italic, woff2 $(embedFile "./src/Arc/Typography/woff2/SourceSerif4Variable-Italic.otf.woff2"))
    --     ]
    -- face
    --     "Source Code"
    --     [ (normal, woff2 $(embedFile "./src/Arc/Typography/woff2/SourceCodeVariable-Roman.otf.woff2"))
    --     , (italic, woff2 $(embedFile "./src/Arc/Typography/woff2/SourceCodeVariable-Italic.otf.woff2"))
    --     ]
    face
        "Inter"
        "https://fonts.googleapis.com/css2?family=Inter:wght@100..900&display=swap"
    face
        "Lora"
        "https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400..700;1,400..700&display=swap"
  where
    --srcUrl c = key "src" $ mconcat [urlContent c, formatContent "woff2", techContent "variations"]
    srcUrl c = key "src" $ urlContent c
    --woff2 = ("data:application/font-woff2;charset=utf-8;base64," <>) . BS.encodeBase64
    face name content = fontFace $ do
        fontFamily [name] []
        srcUrl content
