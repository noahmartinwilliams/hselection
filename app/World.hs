module World(World(..), defaultWorld) where

import Spider
import Log
import Plant
import Bug

data World = World [Spider] [Plant] [Bug] [LogEntry] deriving(Show, Eq)

defaultWorld :: [Int] -> [Double] -> Int -> Int -> World
defaultWorld  randInts _ cols rows = do
    let (bugs, randInts') = randBugs randInts cols rows 10
        (spiders, randInts'') = randSpiders randInts' cols rows 4
        plants = randPlants randInts'' cols rows 20
    World spiders plants bugs  []
