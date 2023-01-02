{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Layouts.Grid where

import Control.Monad (forM)
import Reflex.Dom

class Grid g a where
    gridItems :: [g]
    gridElement :: DomBuilder t m => m (Event t a) -> m (Event t a)
    gridElement = elClass "span" "cell"
    gridContent :: DomBuilder t m => g -> m (Event t a)

grid
    :: forall g a t m
     . Grid g a
    => DomBuilder t m
    => m (Event t a)
grid = divClass "grid" $ do
    cells <- forM (gridItems @g @a) $ \g -> do
        gridElement @g @a $ gridContent @g @a g
    return $ leftmost cells
