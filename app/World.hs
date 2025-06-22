module World(World(..), defaultWorld) where

import Spider
import Log
import Plants

data World = World [Spider] [Plant] [LogEntry] deriving(Show, Eq)

defaultWorld :: [Int] -> [Double] -> World
defaultWorld  _ _ = World [Spider (10, 10) 15, Spider (20, 5) 15] [Plant (15, 15) 5, Plant (16, 17) 10][]
