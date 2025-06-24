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

type RunnerM s w a = StateT s (Writer w) a

spiderDecideAttack ::  Spider -> RunnerM World [LogEntry] Spider
spiderDecideAttack spider = do
    (World spiders plants bugLs logs) <- get
    let bugDists = map (\bug -> getDist (bugPosn bug) (getSpiderPos spider)) bugLs
        bugZipped = zip bugDists bugLs
        bugSorted = sort bugZipped
    if ((length bugSorted) /= 0) && ((fst (bugSorted !! 0)) < 7.0)  && ((fst (bugSorted !! 0)) > 1.5) 
    then do
        lift (tell [SpiderAttacking (getSpiderPos spider)])
        return (SpiderAttack (getSpiderPos spider) (bugPosn (snd (bugSorted !! 0))) (getSpiderEnergy spider))
    else if ((length bugSorted) /= 0) && ((fst (bugSorted !! 0)) <= 1.5)
    then do
        lift (tell [SpiderAteBug (getSpiderPos spider)])
        put (World spiders plants (drop 1 (map snd bugSorted )) logs)
        return (Spider (getSpiderPos spider) (getSpiderEnergy spider))
    else
        return (Spider (getSpiderPos spider) (getSpiderEnergy spider))
        
    
runSpider :: Pos -> Int -> Int -> Double -> Spider -> RunnerM World [LogEntry] Spider
runSpider (tx, ty) cols rows maxSpeed spiderInput | (spiderIsAttacking spiderInput) == False = do
    let (spider@(Spider pos@(cx, cy) energy)) = spiderInput
    let dist = getDist (tx, ty) pos
        newX = (fromIntegral (tx - cx) :: Double) * maxSpeed / dist
        newY = (fromIntegral (ty - cy) :: Double) * maxSpeed / dist
        newPos = (cx + (round newX :: Int), cy + (round newY :: Int))
    newPos' <- lift (adjustPosLog newPos cols rows (SpiderBounced newPos))
    spider' <- lift (decSpiderEnergy (Spider newPos' (getSpiderEnergy spider)) (SpiderStarved newPos'))
    spider'' <- spiderDecideAttack spider'
    return spider''

runSpider _ cols rows maxSpeed spider@(SpiderAttack cpos@(cx, cy) tpos@(tx, ty) energy) = do
    let dist = getDist cpos tpos
        (vx, vy) = (tx - cx, ty - cy)
        (newX, newY) = ((maxSpeed / dist) * (fromIntegral vx :: Double), (maxSpeed/dist) * (fromIntegral vy :: Double))
        newPos = (round newX , round newY)
    newPos' <- lift (adjustPosLog newPos cols rows (SpiderBounced newPos))
    spider' <- spiderDecideAttack (SpiderAttack newPos' tpos energy)
    spider'' <- lift (decSpiderEnergy spider' (SpiderStarved newPos'))
    return spider''

drawSpider :: Spider -> Command
drawSpider (Spider p _) = DrawSpider p
drawSpider (SpiderAttack p _ _ ) = DrawSpiderAttacking p

runSpiders :: [Int] -> [Bug] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
runSpiders rands bugLs cols rows maxSpeed = do
    world@(World spiders plants bugs log) <- get
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
    (World _ _ bugs' _) <- get
    put (World spiders3 plants bugs' (log ++ log'))
    return spiderCommands

runPlants :: [Int] -> RunnerM World [LogEntry] [Command]
runPlants _ = do
    (World _ plantLs _ _) <- get
    return (foldr (++) [] (map drawPlant plantLs))

runBug :: Int -> Int -> Bug -> RunnerM World [LogEntry] Bug
runBug cols rows bug = do
    (World spiders plants bugs _) <- get
    let (bug', logs') = runWriter (obeyGenes cols rows spiders plants bug ) 
    let dists = map (\x -> getDist (getPlantPos x) (bugPosn bug)) plants
        zipped = zip dists plants
        sorted = sort zipped
        bugE = bugEnergy bug
    lift (tell logs')
    if ((length sorted) /= 0) && ((fst (sorted !! 0)) <= 1.5)
    then do
        lift (tell [BugAte (getPlantPos (snd (sorted !! 0))) (getPlantEnergy (snd (sorted !! 0)))])
        put (World spiders (drop 1 (map snd sorted)) bugs logs')
        return (bug' { bugEnergy = bugE + (getPlantEnergy (snd (sorted !! 0))) })
    else
        return bug'

runBugs :: [Int] -> Int -> Int -> RunnerM World [LogEntry] [Command]
runBugs _ cols rows = do
    world@(World _ _ bugs logs) <- get
    let buggedWorld = mapM (runBug cols rows) bugs
    let (_, logs') = runWriter (runStateT buggedWorld world)
    bugs' <- buggedWorld 
    let commands = map (\x -> DrawBug (bugPosn x) ) bugs'
    lift (tell logs')
    (World spiders' plants' _ _) <- get
    put (World spiders' plants' bugs' (logs ++ logs')) 
    return commands

run :: [Int] -> [Double] -> Int -> Int -> Double -> RunnerM World [LogEntry] [Command]
run randInts randDoubles cols rows maxSpeed = do
    (World spiders _ bugs logs) <- get
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
        bugs' `par` (put (World spiders' plants' bugs' (drop logLengthDiff logs')))
        rest <- run randInts' randDoubles' cols rows maxSpeed
        return (commands ++ rest)
    else do
        rest <- run randInts' randDoubles' cols rows maxSpeed
        return (commands ++ rest)
