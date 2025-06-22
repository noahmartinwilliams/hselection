module Run where

import Spider
import World
import Commands
import Pos
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Identity
import Log

type RunnerM s w a = StateT s (Writer w) a

runSpider :: Pos -> Int -> Int -> Double -> Writer [LogEntry] Spider -> Writer [LogEntry] Spider
runSpider (tx, ty) cols rows maxSpeed spiderInput= do
    let (spider@(Spider pos@(cx, cy) energy), _) = runWriter spiderInput
    let dist = getDist (tx, ty) pos
        newX = (fromIntegral (tx - cx) :: Double) * maxSpeed / dist
        newY = (fromIntegral (ty - cy) :: Double) * maxSpeed / dist
        (bounced, newPos) = adjustPos (round newX :: Int, round newY :: Int) cols rows
    if bounced
    then do
        tell [SpiderBounced newPos]
        return spider 
    else
        return (Spider newPos energy)

runSpiders :: [Int] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
runSpiders rands cols rows maxSpeed = do
    world@(World spiders log) <- get
    let numSpiders = length spiders
        randsX = take numSpiders rands
        randsY = take numSpiders (drop numSpiders rands)
        coords = zip randsX randsY
        coordSpiders = zip coords (map (\x -> writer (x, [])) spiders)
        spiders' = mapM (\(coord, spider) -> runSpider coord cols rows maxSpeed spider ) coordSpiders
        (spiders'', log') = runWriter spiders' 
        spiderCommands = map (\(Spider pos _) -> DrawSpider pos) spiders''
    put (World spiders'' log')
    return spiderCommands

run :: [Int] -> [Double] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
run randInts randDoubles cols rows maxSpeed = do
    (World _ logs) <- get
    spiders <- runSpiders randInts cols rows maxSpeed
    let numSpiders = length spiders
        randInts' = drop (2 * numSpiders) randInts
        randDoubles' = drop (2 * numSpiders) randDoubles
    rest <- run randInts' randDoubles' cols rows maxSpeed
    return (spiders ++ [RefreshScr, Wait 1000000, ClrScr] ++ rest)
