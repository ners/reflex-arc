module Arc.Tokens.Size where

import Arc.Util

data SizeToken = SmallSize | MediumSize | LargeSize
    deriving (Bounded, Enum, Show)

instance ClassName SizeToken where
    className SmallSize = "small"
    className MediumSize = "medium"
    className LargeSize = "large"
