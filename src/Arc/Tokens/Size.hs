module Arc.Tokens.Size where

data SizeToken = SmallSize | MediumSize | LargeSize

instance Show SizeToken where
    show SmallSize = "small"
    show MediumSize = "medium"
    show LargeSize = "large"
