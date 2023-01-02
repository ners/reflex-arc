{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Arc.Layouts.Tabs where

import Arc.Widgets.Nav (Nav, nav)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<&>))
import Reflex.Dom

class Nav n => Tabs n where
    initialTab :: Maybe n
    initialTab = Nothing
    tabView
        :: forall t m
         . DomBuilder t m
        => PostBuild t m
        => MonadHold t m
        => Dynamic t (Maybe n)
        -> m ()

tabs
    :: forall n t m
     . Tabs n
    => DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m ()
tabs = divClass "tabs" $ mdo
    tabs' <- nav @n @t @m currentTab <&> (Just <$>)
    currentTab <- holdDyn (initialTab @n) tabs'
    elClass "div" "detail" $ tabView @n currentTab
