module Bug(Bug(..), randBugs, obeyGenes) where

import Pos
import Commands
import Log
import Control.Monad.Writer
import Spider
import Data.List
import Plant

data Bug = Bug { bugPosn :: Pos, bugEnergy :: Int, bugGenes :: [Gene], bugCurrentGene :: Int, bugScratchPosns :: [Pos], bugScratchDoubles :: [Double] } deriving(Show, Eq, Ord)

data Gene = Up Int | Down Int | Left Int | Right Int | Neg Int | GetNearestSpider Int | GetNearestPlant Int | GetMag Int Int | IfLt Int Int | EndIf | IfGt Int Int | GetX Int Int | GetY Int Int deriving(Show, Eq, Ord)

numGeneCons :: Int
numGeneCons = 12

num2gene :: Int -> Int -> Int -> Gene
num2gene 0 i _ = Up i
num2gene 1 i _ = Down i
num2gene 2 i _ = Bug.Left i
num2gene 3 i _ = Bug.Right i
num2gene 4 i _ = Neg i
num2gene 5 i _ = GetNearestSpider i
num2gene 6 i _ = GetNearestPlant i
num2gene 7 i j = GetMag i j
num2gene 8 i j = IfLt i j
num2gene 9 _ _ = EndIf
num2gene 10 i j = IfGt i j
num2gene 11 i j = GetX i j
num2gene 12 i j = GetY i j

obeyGenes :: Int -> Int -> [Spider] -> [Plant] -> Bug -> Writer [LogEntry] Bug
obeyGenes cols rows spiders plants bug@(Bug { bugCurrentGene = x, bugGenes = y}) | (x == ((length y) - 1)) = obeyGenes cols rows spiders plants (bug {bugCurrentGene = 0}) 
obeyGenes cols rows spiderLs plantLs bug@(Bug { bugCurrentGene = i, bugEnergy = e, bugGenes = g, bugPosn = p@(x, y), bugScratchPosns = scratchPosns, bugScratchDoubles = scratchDoubles }) = do
    let currentGene' = i+1
    case (g !! i) of
        Up amt -> do
            newPos <- adjustPosLog (x, y+(round (scratchDoubles !! amt))) cols rows (BugBounced p)
            return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

        Down amt -> do
            newPos <- adjustPosLog (x, y-(round (scratchDoubles !! amt))) cols rows (BugBounced p)
            return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

        Bug.Left amt -> do
            newPos <- adjustPosLog (x - (round (scratchDoubles !! amt)), y) cols rows (BugBounced p)
            return (bug{bugEnergy = (e - 1), bugPosn = newPos, bugCurrentGene = currentGene'})

        Bug.Right amt -> do
            newPos <- adjustPosLog (x+(round (scratchDoubles !! amt)), y) cols rows (BugBounced p)
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

        Neg indx ->
            obeyGenes cols rows spiderLs plantLs (bug { bugCurrentGene = currentGene', bugScratchDoubles = (replaceInList scratchDoubles indx (- (scratchDoubles !! indx)) ) } )

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
        genes = mkRandGenes rest 100 100 100
        posns = take 100 (repeat (0, 0))
        scratch = take 100 (repeat 0.0)
        rest' = drop 100 rest
    (Bug { bugPosn = (randX', randY'), bugEnergy = energy, bugGenes = genes, bugScratchPosns = posns, bugCurrentGene = 0, bugScratchDoubles = scratch}, rest')

mkRandGenes :: [Int] -> Int -> Int -> Int -> [Gene]
mkRandGenes _ 0 _ _ = []
mkRandGenes ( i : j : k : rest ) n nDoubles nVects = (num2gene (mod (abs i) numGeneCons) (mod (abs i) nDoubles) (mod (abs k) nVects) ) : (mkRandGenes rest (n - 1) nDoubles nVects)

randBugs :: [Int] -> Int -> Int -> Int -> ([Bug], [Int])
randBugs rest _ _ 0 = ([], rest)
randBugs randLs cols rows numBugs = do
    let (bug, randLs') = randBug randLs cols rows 100
        (bugs, randLs'') = randBugs randLs' cols rows (numBugs - 1)
    (bug : bugs, randLs'')
