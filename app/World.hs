module World where

import Spider
import Log

data World = World [Spider] [LogEntry] deriving(Show, Eq)

defaultWorld :: [Int] -> [Double] -> World
defaultWorld  _ _ = World [Spider (10, 10) 15, Spider (20, 5) 15] []
