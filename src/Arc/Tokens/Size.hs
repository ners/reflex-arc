module Arc.Tokens.Size where

data Size
    = Em Float
    | Px Float
    | Percent Float

instance Show Size where
    show (Em s) = show s <> "em"
    show (Px s) = show s <> "px"
    show (Percent s) = show s <> "percent"

class IsSize a where
    size :: a -> Size
