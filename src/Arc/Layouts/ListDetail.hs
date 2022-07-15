{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Arc.Layouts.ListDetail where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Default (Default)
import Data.Maybe (fromMaybe)
import Reflex.Dom

class ListDetail l where
    listInitial :: Maybe l
    listInitial = Nothing
    listView :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m) => Dynamic t (Maybe l) -> m (Event t (Maybe l))
    detailView :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m) => Dynamic t (Maybe l) -> m ()

listDetail :: forall l w. ListDetail l => Widget w ()
listDetail = divClass "list-detail" $ mdo
    list <- elClass "aside" "list" $ listView @l listCurrent
    listCurrent <- holdDyn (listInitial @l) list
    elClass "article" "detail" $ detailView @l listCurrent
