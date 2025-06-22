module Run where

import Spider
import World
import Commands
import Pos
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Identity
import Log
import Plant
import Bug

type RunnerM s w a = StateT s (Writer w) a

runSpider :: Pos -> Int -> Int -> Double -> Writer [LogEntry] Spider -> Writer [LogEntry] Spider
runSpider (tx, ty) cols rows maxSpeed spiderInput= do
    let (spider@(Spider pos@(cx, cy) energy), _) = runWriter spiderInput
    let dist = getDist (tx, ty) pos
        newX = (fromIntegral (tx - cx) :: Double) * maxSpeed / dist
        newY = (fromIntegral (ty - cy) :: Double) * maxSpeed / dist
        (bounced, newPos) = adjustPos (round newX :: Int, round newY :: Int) cols rows
    if energy == 0 
    then do
        tell [SpiderStarved pos]
        return spider
    else
        if bounced
        then do
            tell [SpiderBounced newPos]
            return spider 
        else
            return (Spider newPos (energy - 1))

runSpiders :: [Int] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
runSpiders rands cols rows maxSpeed = do
    world@(World spiders plants bugs log) <- get
    let numSpiders = length spiders
        randsX = take numSpiders rands
        randsY = take numSpiders (drop numSpiders rands)
        coords = zip randsX randsY
        coordSpiders = zip coords (map (\x -> writer (x, [])) spiders)
        spiders' = mapM (\(coord, spider) -> runSpider coord cols rows maxSpeed spider ) coordSpiders
        (spiders'', log') = runWriter spiders' 
        spiderCommands = map (\(Spider pos _) -> DrawSpider pos) spiders''
    put (World spiders'' plants bugs (log ++ log'))
    return spiderCommands

runPlants :: [Int] -> RunnerM World [LogEntry] [Command]
runPlants _ = do
    (World _ plantLs _ _) <- get
    return (foldr (++) [] (map drawPlant plantLs))

runBugs :: [Int] -> Int -> Int -> RunnerM World [LogEntry] [Command]
runBugs _ cols rows = do
    (World spiders plants bugs logs) <- get
    let (bugs', logs') = runWriter (mapM (obeyGenes cols rows spiders) bugs)
    let (commands, logs'') = runWriter (mapM (drawBug cols rows) bugs)
    put (World spiders plants bugs' (logs ++ logs' ++ logs''))
    return (foldr (++) [] commands)

run :: [Int] -> [Double] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
run randInts randDoubles cols rows maxSpeed = do
    (World spiders plants bugs logs) <- get
    spidersCommands <- runSpiders randInts cols rows maxSpeed
    plantsCommands <- runPlants randInts
    bugsCommands <- runBugs randInts cols rows
    let numSpiders = length spiders
        randInts' = drop (2 * numSpiders) randInts
        randDoubles' = drop (2 * numSpiders) randDoubles
        logCommands = drawLogs logs cols rows
    (World spiders' plants' bugs' logs') <- get
    let logLengthDiff = abs ((length logs') - (length logs))
        commands = (spidersCommands ++ plantsCommands ++ bugsCommands ++ logCommands ++ [RefreshScr, Wait 1000000, ClrScr])
    if (length logs) >= rows
    then do
        put (World spiders' plants' bugs' (drop logLengthDiff logs'))
        rest <- run randInts' randDoubles' cols rows maxSpeed
        return (commands ++ rest)
    else do
        rest <- run randInts' randDoubles' cols rows maxSpeed
        return (commands ++ rest)
