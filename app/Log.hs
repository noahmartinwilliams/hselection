module Log where

import Pos

data LogEntry = SpiderBounced Pos | SpiderStarved Pos deriving(Show, Eq)

