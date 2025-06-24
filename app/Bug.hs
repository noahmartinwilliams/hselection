module Bug(randBugs, obeyGenes, bugAdjustPosn) where

import Pos
import Commands
import Log
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Identity
import Spider
import Data.List
import Plant
import Types

numGeneCons :: Int
numGeneCons = 16

num2gene :: Int -> Int -> Int -> Gene
num2gene 0 i _ = Up i
num2gene 1 i _ = Down i
num2gene 2 i _ = Types.Left i
num2gene 3 i _ = Types.Right i
num2gene 4 i _ = Neg i
num2gene 5 i _ = GetNearestSpider i
num2gene 6 i _ = GetNearestPlant i
num2gene 7 i j = GetMag i j
num2gene 8 i j = IfLt i j
num2gene 9 _ _ = EndIf
num2gene 10 i j = IfGt i j
num2gene 11 i j = GetX i j
num2gene 12 i j = GetY i j
num2gene 13 _ _ = NOP
num2gene 14 i _ = Reproduce ((mod i 3) + 1)
num2gene 15 i _ = Reproduce ((mod i 3) + 1)

obeyGenes :: Bug -> RunnerM World [LogEntry] (Bool, Bug)
obeyGenes bug@(Bug { bugCurrentGene = x, bugGenes = y}) | (x == ((length y) - 1)) = obeyGenes (bug {bugCurrentGene = 0}) 
obeyGenes bug@(Bug { bugCurrentGene = i, bugEnergy = e, bugGenes = g, bugPosn = p@(x, y), bugScratchPosns = scratchPosns, bugScratchDoubles = scratchDoubles }) = do
    (World cols rows randLs spiderLs plantLs _ _) <- get
    let currentGene' = i+1
        (randInt : _) = take 1 randLs
    case (g !! i) of
        Up amt -> do
            newPos <- adjustPosLog (x, y+(round (scratchDoubles !! amt))) cols rows (BugBounced p)
            return (False, (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'}))

        Down amt -> do
            newPos <- adjustPosLog (x, y-(round (scratchDoubles !! amt))) cols rows (BugBounced p)
            return (False, (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'}))

        Types.Left amt -> do
            newPos <- adjustPosLog (x - (round (scratchDoubles !! amt)), y) cols rows (BugBounced p)
            return (False, (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'}))

        Types.Right amt -> do
            newPos <- adjustPosLog (x+(round (scratchDoubles !! amt)), y) cols rows (BugBounced p)
            return (False, (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'}))

        GetNearestSpider scratchIndex -> do
            let spiderDists = map (\spider -> getDist (getSpiderPos spider) p) spiderLs
                spiderZipped = zip spiderDists spiderLs
                sorted = sort spiderZipped
            if (length sorted) == 0
            then
                (obeyGenes (bug { bugCurrentGene = currentGene'}))
            else
                obeyGenes (bug { bugCurrentGene = currentGene', bugScratchPosns = (replaceInList scratchPosns scratchIndex (getSpiderPos (snd (sorted !! 0)))) })

        GetNearestPlant scratchIndex -> do
            let plantDists = map (\plant -> getDist (getPlantPos plant) p) plantLs
                plantZipped = zip plantDists plantLs
                sorted = sort plantZipped
            if (length sorted) == 0
            then
                obeyGenes (bug { bugCurrentGene = currentGene'})
            else
                obeyGenes (bug { bugCurrentGene = currentGene', bugScratchPosns = (replaceInList scratchPosns scratchIndex (getPlantPos (snd (sorted !! 0)))) })

        GetMag posnIndex doubleIndex -> 
            obeyGenes (bug {bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndex (getDist ( scratchPosns !! posnIndex)  (0, 0))) } )

        IfLt doubleIndx1 doubleIndx2 ->
            if (scratchDoubles !! doubleIndx1) < (scratchDoubles !! doubleIndx2) 
            then
                obeyGenes (bug { bugCurrentGene = currentGene'})
            else
                obeyGenes (bug { bugCurrentGene = (i + (skipToEndIf (drop (i + 1) g)))})

        IfGt doubleIndx1 doubleIndx2 ->
            if (scratchDoubles !! doubleIndx1) > (scratchDoubles !! doubleIndx2) 
            then
                obeyGenes (bug { bugCurrentGene = currentGene'})
            else
                obeyGenes (bug { bugCurrentGene = (i + (skipToEndIf (drop (i + 1) g)))})

        GetX vectIndx doubleIndx ->
            let (x, _) = (scratchPosns !! vectIndx) in obeyGenes (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndx (fromIntegral x :: Double))})

        GetY vectIndx doubleIndx ->
            let (_, y) = (scratchPosns !! vectIndx) in obeyGenes (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndx (fromIntegral y :: Double))})

        EndIf ->
            obeyGenes (bug { bugCurrentGene = currentGene' } )

        Neg indx ->
            obeyGenes (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles indx (- (scratchDoubles !! indx)) ) } )

        Reproduce prob ->
            if (mod (abs randInt) prob) == 0
            then do
                mutatedBug <- (mutate (bug { bugCurrentGene = currentGene'}))
                (_, ret) <- obeyGenes mutatedBug
                return (True, ret)
            else
                obeyGenes (bug { bugCurrentGene = currentGene' })
        NOP ->
            obeyGenes (bug { bugCurrentGene = currentGene' })

mutate :: Bug -> RunnerM World [LogEntry] Bug
mutate bug@(Bug { bugGenes = genes, bugPosn = posn, bugScratchPosns = posns, bugScratchDoubles = scratchDoubles }) = do
    (World _ _ rands _ _ _ _ ) <- get
    tell [BugMutated posn]
    return (bug { bugGenes = (mutateIntern rands genes (length posns) (length scratchDoubles)) }) where

        mutateIntern :: [Int] -> [Gene] -> Int -> Int -> [Gene]
        mutateIntern (pos : randGene : arg1 : arg2 : _) genes posns doubles = do
            let m = min posns doubles
                f x = mod (abs x) m
                randGene' = num2gene randGene (f arg1) (f arg2)
            replaceInList genes pos randGene' 

skipToEndIf :: [Gene] -> Int 
skipToEndIf [] = 0
skipToEndIf (EndIf : _) = 1
skipToEndIf (_ : rest) = 1 + (skipToEndIf rest)

replaceInList :: [a] -> Int -> a -> [a]
replaceInList [] _ _ = []
replaceInList (_ : rest) 0 r = r : rest
replaceInList (x : rest) n r = x : (replaceInList rest (n - 1) r)

randBug :: [Int] -> Int -> Int -> Int -> (Bug, [Int])
randBug (randX : randY : rest ) cols rows energy = do
    let randX' = mod (abs randX) cols
        randY' = mod (abs randY) rows
        genes = mkGenes rest 100 100 100
        posns = take 100 (repeat (0, 0))
        scratch = take 100 (repeat 2.0)
        rest' = drop 100 rest
    (Bug { bugPosn = (randX', randY'), bugEnergy = energy, bugGenes = genes, bugScratchPosns = posns, bugCurrentGene = 0, bugScratchDoubles = scratch}, rest')

mkGenes :: [Int] -> Int -> Int -> Int -> [Gene]
mkGenes randLs numGenes numDoubles numVects = do
    let genes = mkRandGenes randLs numGenes numDoubles numVects
    if genesContainMovement genes
    then
        genes
    else 
        mkGenes (drop 100 randLs) numGenes numDoubles numVects

isMovement :: Gene -> Bool
isMovement (Up _) = True
isMovement (Down _) = True
isMovement (Types.Left _) = True
isMovement (Types.Right _) = True
isMovement _ = False

genesContainMovement :: [Gene] -> Bool
genesContainMovement geneLs = do
    let filtered = filter (isMovement) geneLs
    (length filtered) /= 0

mkRandGenes :: [Int] -> Int -> Int -> Int -> [Gene]
mkRandGenes _ 0 _ _ = []
mkRandGenes ( i : j : k : rest ) n nDoubles nVects = (num2gene (mod (abs i) numGeneCons) (mod (abs i) nDoubles) (mod (abs k) nVects) ) : (mkRandGenes rest (n - 1) nDoubles nVects)

randBugs :: [Int] -> Int -> Int -> Int -> ([Bug], [Int])
randBugs rest _ _ 0 = ([], rest)
randBugs randLs cols rows numBugs = do
    let (bug, randLs') = randBug randLs cols rows 100
        (bugs, randLs'') = randBugs randLs' cols rows (numBugs - 1)
    (bug : bugs, randLs'')

bugAdjustPosn :: Bug -> Int -> Int -> Bug
bugAdjustPosn bug@(Bug { bugPosn = posn }) cols rows = do
    let (x, y) = posn
    bug { bugPosn = (try [(0, -1), (0, 1), (-1, 0), (1, 0)] posn) } where

    try :: [Pos] -> Pos -> Pos
    try ((x, y) : rest) (x', y') = let (bool, newPos) = adjustPos (x+x', y+y') cols rows in if bool then try rest (x', y') else newPos
