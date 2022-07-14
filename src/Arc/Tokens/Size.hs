module Arc.Tokens.Size where

data SizeToken = SmallSize | MediumSize | LargeSize
    deriving (Bounded, Enum)

instance Show SizeToken where
    show SmallSize = "small"
    show MediumSize = "medium"
    show LargeSize = "large"