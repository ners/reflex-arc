{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

between :: Text -> Text -> Text -> Text
between a b = fst . Text.breakOn b . snd . Text.breakOnEnd a

camelCase :: [Text] -> Text
camelCase [] = ""
camelCase (x : xs) = Text.toLower x <> pascalCase xs

pascalCase :: [Text] -> Text
pascalCase = Text.concat . fmap p
  where
    p = (\x -> Text.cons (toUpper $ Text.head x) $ Text.tail x) . Text.toLower

handleSvg :: Text -> IO ()
handleSvg path = do
    let filename = last $ Text.splitOn "/" path
    if filename `elem` ["trash-can.svg", "trophy.svg", "star.svg", "alert-rhombus.svg", "account-circle.svg", "flare.svg"]
        then do
            svg <- Text.readFile $ Text.unpack path
            let id = between "id=\"" "\"" svg
            let viewBox = between "viewBox=\"" "\"" svg
            let path = between "path d=\"" "\"" svg
            let functionName = camelCase $ Text.splitOn "-" id
            Text.putStrLn $
                Text.unlines $
                    Text.unwords
                        <$> [ [functionName, "::", "DomBuilder t m", "=>", "m ()"]
                            , [functionName, "=", "mkMdiIcon", "\"" <> viewBox <> "\"", "\"" <> path <> "\""]
                            ]
        else return ()

main :: IO ()
main = Text.getContents >>= mapM_ handleSvg . Text.lines
