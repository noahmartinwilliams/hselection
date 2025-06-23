module Bug(Bug(..), randBugs, obeyGenes) where

import Pos
import Commands
import Log
import Control.Monad.Writer
import Spider
import Data.List
import Plant

data Bug = Bug { bugPosn :: Pos, bugEnergy :: Int, bugGenes :: [Gene], bugCurrentGene :: Int, bugScratchPosns :: [Pos], bugScratchDoubles :: [Double] } deriving(Show, Eq, Ord)

data Gene = Up | Down | Left | Right | GetNearestSpider Int | GetNearestPlant Int | GetMag Int Int | IfLt Int Int | EndIf | IfGt Int Int | GetX Int Int | GetY Int Int deriving(Show, Eq, Ord)

obeyGenes :: Int -> Int -> [Spider] -> [Plant] -> Bug -> Writer [LogEntry] Bug
obeyGenes cols rows spiders plants bug@(Bug { bugCurrentGene = x, bugGenes = y}) | (x == ((length y) - 1)) = obeyGenes cols rows spiders plants (bug {bugCurrentGene = 0}) 
obeyGenes cols rows spiderLs plantLs bug@(Bug { bugCurrentGene = i, bugEnergy = e, bugGenes = g, bugPosn = p@(x, y), bugScratchPosns = scratchPosns, bugScratchDoubles = scratchDoubles }) = do
    let currentGene' = i+1
    case (g !! i) of
        Up -> do
            newPos <- adjustPosLog (x, y+1) cols rows (BugBounced p)
            return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

        Down -> do
            newPos <- adjustPosLog (x, y-1) cols rows (BugBounced p)
            return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

        Bug.Left -> do
            newPos <- adjustPosLog (x-1, y) cols rows (BugBounced p)
            return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

        Bug.Right -> do
            newPos <- adjustPosLog (x+1, y) cols rows (BugBounced p)
            return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

        GetNearestSpider scratchIndex -> do
            let spiderDists = map (\spider -> getDist (getSpiderPos spider) p) spiderLs
                spiderZipped = zip spiderDists spiderLs
                sorted = sort spiderZipped
            if (length sorted) == 0
            then
                (obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene'}))
            else
                obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene', bugScratchPosns = (replaceInList scratchPosns scratchIndex (getSpiderPos (snd (sorted !! 0)))) })

        GetNearestPlant scratchIndex -> do
            let plantDists = map (\plant -> getDist (getPlantPos plant) p) plantLs
                plantZipped = zip plantDists plantLs
                sorted = sort plantZipped
            if (length sorted) == 0
            then
                obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene'})
            else
                obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene', bugScratchPosns = (replaceInList scratchPosns scratchIndex (getPlantPos (snd (sorted !! 0)))) })

        GetMag posnIndex doubleIndex -> 
            obeyGenes cols rows spiderLs plantLs (bug {bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndex (getDist ( scratchPosns !! posnIndex)  (0, 0))) } )

        IfLt doubleIndx1 doubleIndx2 ->
            if (scratchDoubles !! doubleIndx1) < (scratchDoubles !! doubleIndx2) 
            then
                obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene'})
            else
                obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = (i + (skipToEndIf (drop (i + 1) g)))})

        IfGt doubleIndx1 doubleIndx2 ->
            if (scratchDoubles !! doubleIndx1) > (scratchDoubles !! doubleIndx2) 
            then
                obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene'})
            else
                obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = (i + (skipToEndIf (drop (i + 1) g)))})

        GetX vectIndx doubleIndx ->
            let (x, _) = (scratchPosns !! vectIndx) in obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndx (fromIntegral x :: Double))})

        GetY vectIndx doubleIndx ->
            let (_, y) = (scratchPosns !! vectIndx) in obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles doubleIndx (fromIntegral y :: Double))})

        EndIf ->
            obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene' } )

skipToEndIf :: [Gene] -> Int 
skipToEndIf [] = 0
skipToEndIf (EndIf : _) = 1
skipToEndIf (_ : rest) = 1 + (skipToEndIf rest)

replaceInList :: [a] -> Int -> a -> [a]
replaceInList [] _ _ = []
replaceInList (_ : rest) 0 r = r : rest
replaceInList (x : rest) n r = x : (replaceInList rest (n - 1) r)

randBug :: [Int] -> Int -> Int -> (Bug, [Int])
randBug (randX : randY : rest ) cols rows = do
    let randX' = mod (abs randX) cols
        randY' = mod (abs randY) rows
    (Bug { bugPosn = (randX', randY'), bugEnergy = 15, bugGenes = [GetNearestPlant 0, GetX 0 0, GetY 0 0, IfGt 0 1, Up, EndIf, IfLt 0 1, Down, EndIf ], bugScratchPosns = [(0, 0)], bugCurrentGene = 0, bugScratchDoubles = [0.0, 0.0]}, rest)

randBugs :: [Int] -> Int -> Int -> Int -> ([Bug], [Int])
randBugs rest _ _ 0 = ([], rest)
randBugs randLs cols rows numBugs = do
    let (bug, randLs') = randBug randLs cols rows
        (bugs, randLs'') = randBugs randLs' cols rows (numBugs - 1)
    (bug : bugs, randLs'')
