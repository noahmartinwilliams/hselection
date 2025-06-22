module Log(LogEntry(..), drawLogs) where

import Pos
import Commands

data LogEntry = SpiderBounced Pos | BugBounced Pos | SpiderStarved Pos deriving(Show, Eq)

getCol :: Int -> Int
getCol x = div (2 * x ) 3

drawLog :: LogEntry -> Int -> Int -> Int -> Int -> Command
drawLog logEntry col row cols rows = do
    let (_, newCoord) = adjustPos (col, row) cols rows
    DrawStr (log2str logEntry) newCoord

log2str :: LogEntry -> String
log2str (SpiderBounced pos) = "Spider bounced at: " ++ (pos2string pos) ++ "."
log2str (SpiderStarved pos) = "Spider starved at: " ++ (pos2string pos) ++ "."
log2str (BugBounced pos) = "Bug bounced at: " ++ (pos2string pos) ++ "."

drawLogs :: [LogEntry] -> Int -> Int -> [Command]
drawLogs logLs cols rows = do
    let col = getCol cols
        coords = zip (repeat col) [0..]
    map (\(l, (x, y)) -> drawLog l x y cols rows) (zip logLs coords)
