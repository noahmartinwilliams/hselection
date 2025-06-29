module HSelect.Gene where

import HSelect.Types as Types
import Data.List

numGeneCons :: Int
numGeneCons = 18

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
num2gene 14 i _ = Reproduce ((mod i 2) + 1)
num2gene 15 i _ = Reproduce ((mod i 2) + 1)
num2gene 16 i _ = Reproduce ((mod i 2) + 1)
num2gene 17 i j = Place i j

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
