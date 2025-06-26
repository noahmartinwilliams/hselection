module Run(run) where

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
import Control.Parallel
import Control.Parallel.Strategies
import Par
import Types

spiderDecideAttack ::  Spider -> RunnerM World [LogEntry] Spider
spiderDecideAttack spider = do
    (World cols rows randLs spiders plants bugLs logs) <- get
    let bugDists = parMap rdeepseq (\bug -> getDist (bugPosn bug) (getSpiderPos spider)) bugLs
        bugZipped = zip bugDists bugLs
        bugSorted = sort bugZipped
    if ((length bugSorted) /= 0) && ((fst (bugSorted !! 0)) < 7.0)  && ((fst (bugSorted !! 0)) > 1.5) 
    then do
        lift (tell [SpiderAttacking (getSpiderPos spider)])
        return (SpiderAttack (getSpiderPos spider) (bugPosn (snd (bugSorted !! 0))) (getSpiderEnergy spider))
    else if ((length bugSorted) /= 0) && ((fst (bugSorted !! 0)) <= 1.5)
    then do
        lift (tell [SpiderAteBug (getSpiderPos spider)])
        put (World cols rows randLs spiders plants (drop 1 (map snd bugSorted )) logs)
        return (Spider (getSpiderPos spider) ((getSpiderEnergy spider) + (bugEnergy (snd (bugSorted !! 0)))))
    else
        return (Spider (getSpiderPos spider) (getSpiderEnergy spider))
        
    
runSpider :: Pos -> Int -> Int -> Double -> Spider -> RunnerM World [LogEntry] Spider
runSpider (tx, ty) cols rows maxSpeed spiderInput | (spiderIsAttacking spiderInput) == False = do
    let (spider@(Spider pos@(cx, cy) energy)) = spiderInput
    let dist = getDist (tx, ty) pos
        newX = (fromIntegral (tx - cx) :: Double) * maxSpeed / dist
        newY = (fromIntegral (ty - cy) :: Double) * maxSpeed / dist
        newPos = (cx + (round newX :: Int), cy + (round newY :: Int))
    newPos' <- (adjustPosLog newPos cols rows (SpiderBounced newPos))
    spider' <- lift (decSpiderEnergy (Spider newPos' (getSpiderEnergy spider)) (SpiderStarved newPos'))
    spider'' <- spiderDecideAttack spider'
    return spider''

runSpider _ cols rows maxSpeed spider@(SpiderAttack cpos@(cx, cy) tpos@(tx, ty) energy) = do
    let dist = getDist cpos tpos
        (vx, vy) = (tx - cx, ty - cy)
        (newX, newY) = ((maxSpeed / dist) * (fromIntegral vx :: Double), (maxSpeed/dist) * (fromIntegral vy :: Double))
        newPos = (round newX , round newY)
    newPos' <- (adjustPosLog newPos cols rows (SpiderBounced newPos))
    spider' <- spiderDecideAttack (SpiderAttack newPos' tpos energy)
    spider'' <- lift (decSpiderEnergy spider' (SpiderStarved newPos'))
    return spider''

drawSpider :: Spider -> Command
drawSpider (Spider p _) = DrawSpider p
drawSpider (SpiderAttack p _ _ ) = DrawSpiderAttacking p

runSpiders :: Double -> RunnerM World [LogEntry] [Command]
runSpiders maxSpeed = do
    world@(World cols rows rands spiders plants bugLs log) <- get
    let numSpiders = length spiders
        randsX = take numSpiders rands
        randsY = take numSpiders (drop numSpiders rands)
        coords = zip randsX randsY
        coordSpiders = zip coords spiders 
        spiders' = mapM (\(coord, spider) -> runSpider coord cols rows maxSpeed spider ) coordSpiders
    spiders'' <- spiders'
    let spiders3 = filter (\s -> (getSpiderEnergy s) > 0) spiders''
        spiderCommands = map (\spider -> drawSpider spider) spiders3
        (_, log') = runWriter (runStateT spiders' world)
    (World _ _ _ _ _ bugs' _) <- get
    par3 spiders3 plants bugs' (put (World cols rows rands spiders3 plants bugs' (log ++ log')))
    return spiderCommands

runPlants :: RunnerM World [LogEntry] [Command]
runPlants = do
    (World cols rows rands spiders plantLs bugs logs) <- get
    let [randNum] = take 1 rands
    if (mod (abs randNum) 5) == 0
    then do
        let (newPlants, rands') = randPlants rands cols rows 3
        put (World cols rows rands' spiders (plantLs ++ newPlants) bugs logs)
        lift (tell [PlantsAdded ])
        (World _ _ _ _ plantLs' _ _) <- get
        return (foldr (++) [] (map drawPlant plantLs'))
    else
        return (foldr (++) [] (map drawPlant plantLs))

reproduceBug :: (Bool, Bug) -> Int -> Int -> RunnerM World [LogEntry] Bug
reproduceBug (True, b) cols rows = do
    (World cols rows rands spiders plants bugs logs) <- get
    put (World cols rows rands spiders plants ((bugAdjustPosn b cols rows) : bugs) logs)
    return b
reproduceBug (False, b) _ _ = return b

runBug :: Bug -> RunnerM World [LogEntry] Bug
runBug bug = do
    world@(World cols rows rands spiders plants bugs _) <- get
    let ((bug', _), logs') = runWriter (runStateT (obeyGenes bug ) world)
    bug'' <- reproduceBug bug' cols rows
    let dists = parMap rdeepseq (\x -> getDist (getPlantPos x) (bugPosn bug)) plants
        zipped = zip dists plants
        sorted = sort zipped
        bugE = bugEnergy bug
    lift (tell logs')
    if ((length sorted) /= 0) && ((fst (sorted !! 0)) <= 1.5)
    then do
        lift (tell [BugAte (getPlantPos (snd (sorted !! 0))) (getPlantEnergy (snd (sorted !! 0)))])
        put (World cols rows rands spiders (drop 1 (map snd sorted)) bugs logs')
        return (bug'' { bugEnergy = bugE + (getPlantEnergy (snd (sorted !! 0))) })
    else
        return bug''

runBugs :: RunnerM World [LogEntry] [Command]
runBugs = do
    world@(World cols rows _ _ _ bugs logs) <- get
    let buggedWorld = mapM runBug bugs
    let (_, logs') = runWriter (runStateT buggedWorld world)
    bugs' <- buggedWorld 
    let commands = map (\x -> DrawBug (bugPosn x) ) bugs'
    lift (tell logs')
    (World _ _ rands spiders' plants' _ _) <- get
    par3 spiders' plants' bugs' (put (World cols rows rands spiders' plants' bugs' (logs ++ logs')) )
    return commands

run :: Double -> RunnerM World [LogEntry] [Command]
run maxSpeed = do
    (World cols rows randInts spiders _ bugs logs) <- get
    spidersCommands <- runSpiders maxSpeed
    bugsCommands <- runBugs 
    plantsCommands <- runPlants 
    let numSpiders = length spiders
        randInts' = drop (2 * numSpiders) randInts
        logCommands = drawLogs logs cols rows
    (World _ _ _ spiders' plants' bugs' logs') <- get
    let logLengthDiff = abs ((length logs') - (length logs))
        commands = (spidersCommands ++ plantsCommands ++ bugsCommands ++ logCommands ++ [RefreshScr, Wait 10000, ClrScr])
    if (length logs) >= rows
    then do
        (put (World cols rows randInts' spiders' plants' bugs' (drop logLengthDiff logs')))
        rest <- run maxSpeed
        return (commands ++ rest)
    else do
        rest <- run maxSpeed
        return (commands ++ rest)
