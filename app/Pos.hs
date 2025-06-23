module Pos(Pos(..), adjustPos, adjustPosLog, getDist, pos2string) where

import Control.Monad.Writer

type Pos = (Int, Int)

getDist :: Pos -> Pos -> Double
getDist (x1, y1) (x2, y2) = do
    let x1' = fromIntegral x1 :: Double
        x2' = fromIntegral x2 :: Double
        y1' = fromIntegral y1 :: Double
        y2' = fromIntegral y2 :: Double
        dx = x1' - x2'
        dy = y1' - y2'
    sqrt (dx*dx + dy*dy)

cutOff :: Int -> Int -> Int
cutOff i m | i > m = m
cutOff i _ | i < 0 = 0
cutOff i _ = i

adjustPosLog :: (Int, Int) -> Int -> Int -> a -> Writer [a] Pos
adjustPosLog (x, y) cols rows err = do
    let (b, npos) = adjustPos (x, y) cols rows
    if b
    then do
        tell [err]
        return npos
    else
        return npos 

adjustPos :: (Int, Int) -> Int -> Int -> (Bool, Pos)
adjustPos (x, y) cols rows = do
    if (y > rows) || (y < 0) || (x > cols) || (x < 0) 
    then
        (True, (cutOff x cols, cutOff y rows))
    else
        (False, (x, y))

pos2string :: Pos -> String
pos2string (x, y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"
