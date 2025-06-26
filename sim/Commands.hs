{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Commands where

import Pos
import Control.DeepSeq
import GHC.Generics

data Command = DrawStr String Pos | RefreshScr | ClrScr | Wait Int | DrawSpider Pos | DrawSpiderAttacking Pos | DrawPlant Pos | DrawBug Pos deriving(Show, Eq, Generic, NFData)
