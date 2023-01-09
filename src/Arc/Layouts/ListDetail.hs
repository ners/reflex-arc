{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Arc.Layouts.ListDetail where

import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Reflex.Dom

class ListDetail l where
    listInitial :: Maybe l
    listInitial = Nothing
    listView
        :: forall t m
         . DomBuilder t m
        => PostBuild t m
        => MonadHold t m
        => MonadFix m
        => Dynamic t (Maybe l)
        -> m (Event t (Maybe l))
    detailView
        :: forall t m
         . DomBuilder t m
        => PostBuild t m
        => MonadHold t m
        => MonadFix m
        => Dynamic t (Maybe l)
        -> m ()
    detailClassName
        :: forall t m
         . DomBuilder t m
        => PostBuild t m
        => MonadHold t m
        => MonadFix m
        => Dynamic t (Maybe l)
        -> Dynamic t (Maybe Text)
    detailClassName _ = pure Nothing

listDetail
    :: forall l t m
     . ListDetail l
    => DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m ()
listDetail = divClass "list-detail" $ mdo
    list :: Event t (Maybe l) <- elClass "aside" "list" $ listView @l listCurrent
    listCurrent :: Dynamic t (Maybe l) <- holdDyn (listInitial @l) list
    let cn = detailClassName @l @t @m listCurrent
    elDynClass "article" ("detail" <> (maybe "" (" " <>) <$> cn)) $ detailView @l listCurrent
