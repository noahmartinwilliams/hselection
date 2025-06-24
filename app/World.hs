module World(defaultWorld) where

import Spider
import Log
import Plant
import Bug
import Types

defaultWorld :: [Int] -> [Double] -> Int -> Int -> World
defaultWorld  randInts _ cols rows = do
    let (bugs, randInts') = randBugs randInts (2 * (div cols 3)) rows 10
        (spiders, randInts'') = randSpiders randInts' (2 * (div cols 3)) rows 5
        (plants, randInts3)  = randPlants randInts'' (2 * (div cols 3)) rows 20
    World cols (rows - 2) randInts3 spiders plants bugs []
