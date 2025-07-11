module World(defaultWorld) where

import Spider
import Log
import Plant
import Bug
import HSelect.Types
import HSelect.Gene
import HSelect.Plant
import HSelect.Spider

defaultWorld :: [Int] -> Int -> Int -> World
defaultWorld randInts cols rows = do
    let (bugs, randInts') = randBugs randInts (2 * (div cols 3)) rows 100
        (spiders, randInts'') = randSpiders randInts' (2 * (div cols 3)) rows 75
        (plants, randInts3)  = randPlants randInts'' (2 * (div cols 3)) rows 80
    World cols (rows - 2) randInts3 spiders plants bugs []
