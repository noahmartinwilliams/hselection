module Plant(drawPlant, getPlantPos, getPlantEnergy, randPlants) where

import Pos
import Commands
import Types

drawPlant :: Plant -> [Command]
drawPlant (Plant pos _) = [DrawPlant pos]

getPlantPos :: Plant -> Pos
getPlantPos (Plant p _) = p

getPlantEnergy :: Plant -> Int
getPlantEnergy (Plant _ e) = e

randPlants :: [Int] -> Int -> Int -> Int -> ([Plant], [Int])
randPlants randLs cols rows n = do
    let xs = take n randLs
        ys = take n (drop n randLs)
        xy = zip xs ys
    (map (\(x, y) -> Plant ((mod (abs x) cols), (mod (abs y) rows)) 10) xy, drop (2 * n) randLs)
