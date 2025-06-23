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
import Data.List

type RunnerM s w a = StateT s (Writer w) a

spiderDecideAttack :: [Bug] -> Spider -> Writer [LogEntry] Spider
spiderDecideAttack bugLs spider = do
    let bugDists = map (\bug -> getDist (bugPosn bug) (getSpiderPos spider)) bugLs
        bugZipped = zip bugDists bugLs
        bugSorted = sort bugZipped
    if ((length bugSorted) /= 0) && ((fst (bugSorted !! 0)) < 7.0) 
    then do
        tell [SpiderAttacking (getSpiderPos spider)]
        return (SpiderAttack (getSpiderPos spider) (bugPosn (snd (bugSorted !! 0))) (getSpiderEnergy spider))
    else
        return (Spider (getSpiderPos spider) (getSpiderEnergy spider))
        
    
runSpider :: Pos -> [Bug] -> Int -> Int -> Double -> Spider -> Writer [LogEntry] Spider
runSpider (tx, ty) bugLs cols rows maxSpeed spiderInput | (spiderIsAttacking spiderInput) == False = do
    let (spider@(Spider pos@(cx, cy) energy)) = spiderInput
    let dist = getDist (tx, ty) pos
        newX = (fromIntegral (tx - cx) :: Double) * maxSpeed / dist
        newY = (fromIntegral (ty - cy) :: Double) * maxSpeed / dist
        newPos = (round newX :: Int, round newY :: Int)
    newPos' <- adjustPosLog newPos cols rows (SpiderBounced newPos)
    spider' <- decSpiderEnergy (Spider newPos' (getSpiderEnergy spider)) (SpiderStarved newPos')
    spider'' <- spiderDecideAttack bugLs spider'
    return spider''

runSpider _ bugLs cols rows maxSpeed spider@(SpiderAttack cpos@(cx, cy) tpos@(tx, ty) energy) = do
    let dist = getDist cpos tpos
        (vx, vy) = (tx - cx, ty - cy)
        (newX, newY) = ((maxSpeed / dist) * (fromIntegral vx :: Double), (maxSpeed/dist) * (fromIntegral vy :: Double))
        newPos = (round newX , round newY)
    newPos' <- adjustPosLog newPos cols rows (SpiderBounced newPos)
    spider' <- spiderDecideAttack bugLs (SpiderAttack newPos' tpos energy)
    spider'' <- decSpiderEnergy spider' (SpiderStarved newPos')
    return spider''

runSpiders :: [Int] -> [Bug] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
runSpiders rands bugLs cols rows maxSpeed = do
    world@(World spiders plants bugs log) <- get
    let numSpiders = length spiders
        randsX = take numSpiders rands
        randsY = take numSpiders (drop numSpiders rands)
        coords = zip randsX randsY
        coordSpiders = zip coords spiders 
        spiders' = mapM (\(coord, spider) -> runSpider coord bugLs cols rows maxSpeed spider ) coordSpiders
        (spiders'', log') = runWriter spiders' 
        spiders3 = filter (\s -> (getSpiderEnergy s) > 0) spiders''
        spiderCommands = map (\spider -> DrawSpider (getSpiderPos spider)) spiders3
    tell log'
    put (World spiders3 plants bugs (log ++ log'))
    return spiderCommands

runPlants :: [Int] -> RunnerM World [LogEntry] [Command]
runPlants _ = do
    (World _ plantLs _ _) <- get
    return (foldr (++) [] (map drawPlant plantLs))

runBugs :: [Int] -> Int -> Int -> RunnerM World [LogEntry] [Command]
runBugs _ cols rows = do
    (World spiders plants bugs logs) <- get
    let (bugs', logs') = runWriter (mapM (obeyGenes cols rows spiders) bugs)
    let commands = map (\x -> DrawBug (bugPosn x) ) bugs'
    tell logs'
    put (World spiders plants bugs' (logs ++ logs'))
    return commands

run :: [Int] -> [Double] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
run randInts randDoubles cols rows maxSpeed = do
    (World spiders plants bugs logs) <- get
    spidersCommands <- runSpiders randInts bugs cols rows maxSpeed
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
