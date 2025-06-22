module Commands where

import Pos
data Command = DrawStr String Pos | RefreshScr | ClrScr | Wait Int | DrawSpider Pos deriving(Show, Eq)
