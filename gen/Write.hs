module Write where

import HSelect.Types as Types
import HSelect.Gene
import Control.Parallel.Strategies

pos2str :: Pos -> String
pos2str (x, y) = "<" ++ (show x) ++ ", " ++ (show y) ++ ">"

gene2str :: Gene -> String
gene2str NOP = "nop"
gene2str (Up i) = "up(" ++ (show i) ++ ")"
gene2str (Down i) = "down(" ++ (show i) ++ ")"
gene2str (Types.Left i) = "left(" ++ (show i) ++ ")"
gene2str (Types.Right i) = "right(" ++ (show i) ++ ")"
gene2str (Neg i) = "neg(" ++ (show i) ++ ")"
gene2str (GetNearestSpider i) = "get_nearest_spider(" ++ (show i) ++ ")"
gene2str (GetNearestPlant i) = "get_nearest_plant(" ++ (show i) ++ ")"
gene2str (GetMag i j) = "get_mag(" ++ (show i) ++ ", " ++ (show j) ++ ")"
gene2str (IfLt i j) = "if_lt(" ++ (show i) ++ ", " ++ (show j) ++ ")"
gene2str EndIf = "endif"
gene2str (IfGt i j) = "if_gt(" ++ (show i) ++ ", " ++ (show j) ++ ")"
gene2str (GetX i j) = "get_x(" ++ (show i) ++ ", " ++ (show j) ++ ")"
gene2str (GetY i j) = "get_y(" ++ (show i) ++ ", " ++ (show j) ++ ")"
gene2str (Reproduce i) = "reproduce(" ++ (show i) ++ ")"

genes2str :: [Gene] -> String
genes2str geneLs = do
    let strs = parMap rdeepseq (\x -> ", " ++ (gene2str x) ) geneLs
    drop 2 (foldr (++) "" strs )

scratchPosns2str :: [Pos] -> String
scratchPosns2str ls = let strs = parMap rdeepseq (\x -> ", " ++ (pos2str x)) ls in foldr (++) "" (drop 1 strs)

scratchDoubles2str :: [Double] -> String
scratchDoubles2str ls = let strs = map (\x -> ", " ++ (show x)) ls in foldr (++) "" (drop 1 strs)

bug2str :: Bug -> String
bug2str (Bug { bugPosn = posn, bugEnergy = energy, bugGenes = genes, bugCurrentGene = currentGene, bugScratchPosns = scratchPosns, bugScratchDoubles = scratchDoubles}) = do
    "bug(" ++ (pos2str posn) ++ ", " ++ (show energy) ++ ", [" ++ (genes2str genes) ++ "], [" ++ (scratchPosns2str scratchPosns) ++ "], [" ++ (scratchDoubles2str scratchDoubles) ++ "])."

spider2str :: Spider -> String
spider2str (Spider pos energy) = "spider(" ++ (pos2str pos) ++ ", " ++ (show energy) ++ ")."
spider2str (SpiderAttack pos pos2 energy) = "spider_attack(" ++ (pos2str pos) ++ ", " ++ (pos2str pos2) ++ ", " ++ (show energy) ++ ")."

plant2str :: Plant -> String
plant2str (Plant pos energy) = "plant(" ++ (pos2str pos) ++ ", " ++ (show energy) ++ ")."

world2str :: World -> String
world2str (World cols rows (randInt : _) spiders plants bugs _ ) = do
    let spiders' = parMap rdeepseq (\x -> (spider2str x) ++ "\n") spiders
        plants' = parMap rdeepseq (\x -> (plant2str x) ++ "\n") plants
        bugs' = parMap rdeepseq (\x -> (bug2str x) ++ "\n") bugs
        spiders'' = foldr (++) "" spiders'
        plants'' = foldr (++) "" plants'
        bugs'' = foldr (++) "" bugs'
    "world(" ++ (show cols) ++ ", " ++ (show rows) ++ ", " ++ (show randInt) ++ ").\n" ++ spiders'' ++ plants'' ++ bugs''
