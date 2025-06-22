module Commands where

import Pos
data Command = DrawStr String Pos | RefreshScr | ClrScr | Wait Int | DrawSpider Pos | DrawPlant Pos deriving(Show, Eq)
