{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Widgets.Nav where

import Arc.Util
import Reflex.Dom

class Selectable n => Nav n where
    navElements :: [n]
    default navElements :: (Bounded n, Enum n) => [n]
    navElements = [minBound .. maxBound]
    navElement :: (DomBuilder t m, PostBuild t m) => n -> Dynamic t (Maybe n) -> m (Event t n)
    navElement = selectable

nav
    :: forall n t m
     . Nav n
    => DomBuilder t m
    => PostBuild t m
    => Dynamic t (Maybe n)
    -> m (Event t n)
nav d = el "nav" $
    el "ul" $ do
        elems :: [Event t n] <- mapM (`navElement` d) (navElements @n)
        return $ leftmost elems
