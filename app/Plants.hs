module Plants(Plant(..), drawPlant) where

import Pos
import Commands

data Plant = Plant Pos Int deriving(Show, Eq, Ord)

drawPlant :: Plant -> [Command]
drawPlant (Plant pos _) = [DrawPlant pos]
