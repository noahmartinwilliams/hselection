module Log(drawLogs) where

import Pos
import Commands
import Control.Parallel.Strategies
import Types


getCol :: Int -> Int
getCol x = div (2 * x ) 3

drawLog :: LogEntry -> Int -> Int -> Int -> Int -> Command
drawLog logEntry col row cols rows = do
    let (_, newCoord) = adjustPos (col, row) cols rows
    DrawStr (log2str logEntry) newCoord

log2str :: LogEntry -> String
log2str (SpiderAteBug pos) = "Spider ate bug at: " ++ (pos2string pos) ++ "."
log2str (SpiderBounced pos) = "Spider bounced at: " ++ (pos2string pos) ++ "."
log2str (SpiderStarved pos) = "Spider starved at: " ++ (pos2string pos) ++ "."
log2str (SpiderAttacking pos) = "Spider attacking: " ++ (pos2string pos) ++ "."
log2str (BugBounced pos) = "Bug bounced at: " ++ (pos2string pos) ++ "."
log2str (BugAte pos e) = "Bug ate plant at: " ++ (pos2string pos) ++ " for " ++ (show e) ++ " energy."

drawLogs :: [LogEntry] -> Int -> Int -> [Command]
drawLogs logLs cols rows = do
    let col = getCol cols
        coords = zip (repeat col) [0..]
    parMap rseq (\(l, (x, y)) -> drawLog l x y cols rows) (zip logLs coords)
