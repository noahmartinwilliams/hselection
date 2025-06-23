module Plant(Plant(..), drawPlant, getPlantPos, getPlantEnergy) where

import Pos
import Commands

data Plant = Plant Pos Int deriving(Show, Eq, Ord)

drawPlant :: Plant -> [Command]
drawPlant (Plant pos _) = [DrawPlant pos]

getPlantPos :: Plant -> Pos
getPlantPos (Plant p _) = p

getPlantEnergy :: Plant -> Int
getPlantEnergy (Plant _ e) = e
