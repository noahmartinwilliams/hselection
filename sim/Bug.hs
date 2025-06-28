module Bug(randBugs, obeyGenes, bugAdjustPosn) where

import HSelect.Gene
import Pos
import Commands
import Log
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Identity
import Spider
import Data.List
import Plant
import HSelect.Types as Types
import Control.Parallel.Strategies


obeyGenes :: Bug -> Int -> RunnerM World [LogEntry] (Bool, Bug)
obeyGenes b 0 = return (False, b)
obeyGenes bug@(Bug { bugCurrentGene = x, bugGenes = y}) d | (x == ((length y) - 1)) = obeyGenes (bug {bugCurrentGene = 0}) ( d - 1 )
obeyGenes bug@(Bug { bugCurrentGene = i, bugEnergy = e, bugGenes = g, bugPosn = p@(x, y), bugScratchPosns = scratchPosns, bugScratchDoubles = scratchDoubles }) d = do
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
            let spiderDists = parMap rdeepseq (\spider -> getDist (getSpiderPos spider) p) spiderLs
                spiderZipped = zip spiderDists spiderLs
                sorted = sort spiderZipped
            if (length sorted) == 0
            then
                (obeyGenes (bug { bugCurrentGene = currentGene'}) (d - 1))
            else
                obeyGenes (bug { bugCurrentGene = currentGene', bugScratchPosns = (replaceInList scratchPosns scratchIndex (getSpiderPos (snd (sorted !! 0)))) }) (d - 1)

        GetNearestPlant scratchIndex -> do
            let plantDists = parMap rdeepseq (\plant -> getDist (getPlantPos plant) p) plantLs
                plantZipped = zip plantDists plantLs
                sorted = sort plantZipped
            if (length sorted) == 0
            then
                obeyGenes (bug { bugCurrentGene = currentGene'}) (d - 1)
            else
                obeyGenes (bug { bugCurrentGene = currentGene', bugScratchPosns = (replaceInList scratchPosns scratchIndex (getPlantPos (snd (sorted !! 0)))) }) (d - 1)

        GetMag posnIndex doubleIndex -> 
            obeyGenes (bug {bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndex (getDist ( scratchPosns !! posnIndex)  (0, 0))) } ) (d - 1)

        IfLt doubleIndx1 doubleIndx2 ->
            if (scratchDoubles !! doubleIndx1) < (scratchDoubles !! doubleIndx2) 
            then
                obeyGenes (bug { bugCurrentGene = currentGene'}) (d - 1)
            else
                obeyGenes (bug { bugCurrentGene = (i + (skipToEndIf (drop (i + 1) g)))}) (d - 1)

        IfGt doubleIndx1 doubleIndx2 ->
            if (scratchDoubles !! doubleIndx1) > (scratchDoubles !! doubleIndx2) 
            then
                obeyGenes (bug { bugCurrentGene = currentGene'}) (d - 1)
            else
                obeyGenes (bug { bugCurrentGene = (i + (skipToEndIf (drop (i + 1) g)))}) (d - 1)

        GetX vectIndx doubleIndx ->
            let (x, _) = (scratchPosns !! vectIndx) in obeyGenes (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndx (fromIntegral x :: Double))}) (d - 1)

        GetY vectIndx doubleIndx ->
            let (_, y) = (scratchPosns !! vectIndx) in obeyGenes (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndx (fromIntegral y :: Double))}) (d - 1)

        EndIf ->
            obeyGenes (bug { bugCurrentGene = currentGene' } ) (d - 1)

        Neg indx ->
            obeyGenes (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles indx (- (scratchDoubles !! indx)) ) } ) (d - 1)

        Reproduce prob ->
            if (mod (abs randInt) prob) == 0
            then do
                mutatedBug <- (mutate (bug { bugCurrentGene = currentGene'}))
                (_, ret) <- obeyGenes mutatedBug (d - 1)
                return (True, ret)
            else
                obeyGenes (bug { bugCurrentGene = currentGene' }) (d - 1)
        NOP ->
            obeyGenes (bug { bugCurrentGene = currentGene' }) (d - 1)

        Place pos value ->
            obeyGenes (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles pos (fromIntegral value :: Double))}) (d - 1)

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

bugAdjustPosn :: Bug -> Int -> Int -> Bug
bugAdjustPosn bug@(Bug { bugPosn = posn }) cols rows = do
    let (x, y) = posn
    bug { bugPosn = (try [(0, -1), (0, 1), (-1, 0), (1, 0)] posn) } where

    try :: [Pos] -> Pos -> Pos
    try ((x, y) : rest) (x', y') = let (bool, newPos) = adjustPos (x+x', y+y') cols rows in if bool then try rest (x', y') else newPos
