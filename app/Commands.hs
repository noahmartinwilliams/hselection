module Commands where

import Pos
data Command = RefreshScr | ClrScr | Wait Int | DrawSpider Pos deriving(Show, Eq)
