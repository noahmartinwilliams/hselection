module Plant(drawPlant, getPlantPos, getPlantEnergy) where

import Pos
import Commands
import HSelect.Types

drawPlant :: Plant -> [Command]
drawPlant (Plant pos _) = [DrawPlant pos]

getPlantPos :: Plant -> Pos
getPlantPos (Plant p _) = p

getPlantEnergy :: Plant -> Int
getPlantEnergy (Plant _ e) = e

