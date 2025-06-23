module World(World(..), defaultWorld) where

import Spider
import Log
import Plant
import Bug

data World = World [Spider] [Plant] [Bug] [LogEntry] deriving(Show, Eq)

defaultWorld :: [Int] -> [Double] -> Int -> Int -> World
defaultWorld  randInts _ cols rows = do
    let (bugs, _) = randBugs randInts cols rows 10
    World [Spider (10, 10) 15, Spider (25, 23) 15] [Plant (15, 15) 5, Plant (16, 17) 10] bugs  []
