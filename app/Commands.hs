module Commands where

import Pos
data Command = DrawStr String Pos | RefreshScr | ClrScr | Wait Int | DrawSpider Pos | DrawSpiderAttacking Pos | DrawPlant Pos | DrawBug Pos deriving(Show, Eq)
