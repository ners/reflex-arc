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
        [ (normal, "https://fonts.googleapis.com/css2?family=Inter:wght@100..700&display=swap")
        ]
    face
        "Source Sans"
        [ (normal, "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@100..700&display=swap")
        , (italic, "https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@100..700&display=swap")
        ]
    face
        "Source Serif"
        [ (normal, "https://fonts.googleapis.com/css2?family=Source+Serif+4:wght@200..700&display=swap")
        , (italic, "https://fonts.googleapis.com/css2?family=Source+Serif+4:ital,wght@100..700&display=swap")
        ]
    face
        "Source Code"
        [ (normal, "https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@200..700&display=swap")
        , (italic, "https://fonts.googleapis.com/css2?family=Source+Code+Pro:ital,wght@100..700&display=swap")
        ]
  where
    srcUrl c = key "src" [urlContent c, techContent "variations"]
    --woff2 = ("data:application/font-woff2;charset=utf-8;base64," <>) . BS.encodeBase64
    face name = fmap Prelude.head $
        fmap $ \(style, content) ->
            fontFace $ do
                fontFamily [name] []
                fontStyle style
                srcUrl content
