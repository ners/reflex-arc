{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Arc.Main where

import Arc.Clay.App (appStyle)
import Arc.Clay.Util (renderBS, renderText)

import Control.Concurrent (forkIO, yield)
import Control.Monad (void)
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Network.Mime (FileName, MimeType, defaultMimeLookup, defaultMimeType)
import Reflex.Dom (Widget, attachWidget, withJSContextSingleton, withJSContextSingletonMono)
import System.Directory (getCurrentDirectory)

import Data.GI.Base (AttrOp (..), new, on)
import GI.GLib (idleAdd, timeoutAdd, pattern PRIORITY_DEFAULT, pattern PRIORITY_HIGH)
import GI.GLib.Structs.Bytes (bytesNew)
import qualified GI.Gdk as Gdk
import GI.Gio.Objects (Cancellable, memoryInputStreamNewFromData)
import GI.Gtk (Adjustment)
import qualified GI.Gtk as Gtk
import GI.JavaScriptCore (valueToString)
import GI.WebKit2 (settingsEnableDeveloperExtras)
import qualified GI.WebKit2 as WK
import GI.WebKit2.Objects.Download (downloadGetDestination)
import GI.WebKit2.Structs (
    userScriptNew,
    userStyleSheetNew,
 )
#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
#endif

import JSDOM (currentDocumentUnchecked)
import JSDOM.Generated.Document (getBodyUnchecked, getHeadUnchecked)
import JSDOM.Generated.Element (setInnerHTML)
import Language.Javascript.JSaddle (Batch, JSM, Results)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (ghcjsHelpers, initState, runBatch)

jsaddleJs :: LBS.ByteString
jsaddleJs =
    ghcjsHelpers
        <> mconcat
            [ "runJSaddleBatch = (function() {\n"
            , initState
            , "\nreturn function(batch) {\n"
            , runBatch
                (\a -> "window.webkit.messageHandlers.jsaddle.postMessage(JSON.stringify(" <> a <> "));\n")
                (Just (\a -> "JSON.parse(window.prompt(\"JSaddleSync\", JSON.stringify(" <> a <> ")))"))
            , "};\n"
            , "})()"
            ]

arcFile :: FileName -> (MimeType, BS.ByteString)
arcFile f = maybe notFound (mime,) content
  where
    mime = defaultMimeLookup f
    notFound = (defaultMimeType, "Not found")
    content = case f of
        "/index.html" -> Just ""
        "/jsaddle.js" -> Just $ LBS.toStrict jsaddleJs
        "/style.css" -> Just $ renderBS appStyle

run :: JSM () -> IO ()
run main = do
    void $ Gtk.init Nothing
    win <-
        new
            Gtk.Window
            [ #type := Gtk.WindowTypeToplevel
            , #defaultWidth := 900
            , #defaultHeight := 600
            ]
    ctx <- new WK.WebContext []
    #registerUriScheme ctx "arc" $ \req -> do
        path :: FileName <- #getPath req
        Text.putStrLn $ "ARC request: " <> path
        let (mime, content) = arcFile path
        let mime = defaultMimeLookup path
        stream <- memoryInputStreamNewFromData content Nothing
        #finish req stream (-1) (Just $ Text.decodeUtf8 mime)
    cm <- new WK.UserContentManager []
    style <- userStyleSheetNew (renderText appStyle) WK.UserContentInjectedFramesAllFrames WK.UserStyleLevelUser Nothing Nothing
    script <-
        userScriptNew
            (Text.decodeUtf8 $ LBS.toStrict jsaddleJs)
            WK.UserContentInjectedFramesAllFrames
            WK.UserScriptInjectionTimeStart
            Nothing
            Nothing
    #addStyleSheet cm style
    #addScript cm script
    wv <-
        new
            WK.Settings
            [ #enableDeveloperExtras := True
            , #enableJavascript := True
            , #enableWriteConsoleMessagesToStdout := True
            ]
            >>= \settings ->
                new
                    WK.WebView
                    [ #webContext := ctx
                    , #userContentManager := cm
                    , #settings := settings
                    ]
    void $ timeoutAdd PRIORITY_HIGH 10 (yield >> return True)
    void $ on win #destroy Gtk.mainQuit
    void $ on wv #close $ #destroy win
    void $ on wv #loadChanged $ \event -> putStrLn $ "Load changed: " <> show event
    void $
        on ctx #downloadStarted $ \dl -> do
            uri <- downloadGetDestination dl
            putStrLn $ "Download started: " <> show uri
    void $
        on wv #loadFailed $ \_ uri err -> do
            errMsg <- WK.gerrorMessage err
            putStrLn . Text.unpack $ "Error when reading \"" <> uri <> "\": " <> errMsg
            return True
    void $
        on wv #loadChanged $ \case
            WK.LoadEventFinished -> runInWebView main wv
            _ -> return ()
    installQuitHandler wv
    -- #loadUri wv "arc:///index.html"
    -- runInWebView main wv
    pwd <- getCurrentDirectory
    #loadHtml wv "" . Just $ "file://" <> Text.pack pwd <> "/index.html"
    #add win wv
    #showAll win
    Gtk.main

runInWebView :: JSM () -> WK.WebView -> IO ()
runInWebView f webView = do
    (processResults, syncResults, start) <-
        runJavaScript
            ( \batch ->
                postGUIAsync $
                    #runJavascript webView (Text.decodeUtf8 . LBS.toStrict $ "runJSaddleBatch(" <> Aeson.encode batch <> ");") noCancellable Nothing
            )
            f
    return ()

-- addJSaddleHandler webView processResults syncResults
-- #runJavascript webView (Text.decodeUtf8 $ LBS.toStrict jsaddleJs) noCancellable . Just $
--     \_obj _asyncResult ->
--         void $ forkIO start

addJSaddleHandler :: WK.WebView -> (Results -> IO ()) -> (Results -> IO Batch) -> IO ()
addJSaddleHandler webView processResult syncResults = do
    cm <- #getUserContentManager webView
    _ <- on cm #scriptMessageReceived $ \result -> do
        arg <- #getJsValue result
        bs <- Text.encodeUtf8 <$> valueToString arg
        mapM_ processResult (Aeson.decode (LBS.fromStrict bs))
    _ <- on webView #scriptDialog $ \dialog ->
        #getDialogType dialog >>= \case
            WK.ScriptDialogTypePrompt ->
                #getMessage dialog >>= \case
                    "JSaddleSync" -> do
                        resultsText <- #promptGetDefaultText dialog
                        case Aeson.decode (LBS.fromStrict $ Text.encodeUtf8 resultsText) of
                            Just results -> do
                                batch <- syncResults results
                                #promptSetText dialog (Text.decodeUtf8 . LBS.toStrict $ Aeson.encode batch)
                                return True
                            Nothing -> return False
                    _ -> return False
            _ -> return False
    void $ #registerScriptMessageHandler cm "jsaddle"

installQuitHandler :: WK.WebView -> IO ()
#ifdef mingw32_HOST_OS
installQuitHandler wv = return () -- TODO: Maybe figure something out here for Windows users.
#else
installQuitHandler wv = void $ installHandler keyboardSignal (Catch (quitWebView wv)) Nothing
#endif

postGUIAsync :: IO () -> IO ()
postGUIAsync action =
    void . idleAdd PRIORITY_DEFAULT $ action >> return False

noAdjustment :: Maybe Adjustment
noAdjustment = Nothing

noCancellable :: Maybe Cancellable
noCancellable = Nothing

quitWebView :: WK.WebView -> IO ()
quitWebView wv = postGUIAsync $ #getToplevel wv >>= #destroy

arcMain :: (forall x. Widget x ()) -> IO ()
arcMain w = run $
    withJSContextSingleton $ \jsSing -> do
        doc <- currentDocumentUnchecked
        -- headElement <- getHeadUnchecked doc
        -- setInnerHTML headElement ("<link rel=\"stylesheet\" href=\"style.css\">" :: Text)
        body <- getBodyUnchecked doc
        attachWidget body jsSing w
