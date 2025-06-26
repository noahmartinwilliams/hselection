{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.DeepSeq
import GHC.Generics

type Pos = (Int, Int)

data Bug = Bug { bugPosn :: Pos, bugEnergy :: Int, bugGenes :: [Gene], bugCurrentGene :: Int, bugScratchPosns :: [Pos], bugScratchDoubles :: [Double] } deriving(Show, Eq, Ord, Generic, NFData)

data Gene = NOP | Up Int | Down Int | Left Int | Right Int | Neg Int | GetNearestSpider Int | GetNearestPlant Int | GetMag Int Int | IfLt Int Int | EndIf | IfGt Int Int | GetX Int Int | GetY Int Int | Reproduce Int deriving(Show, Eq, Ord, Generic, NFData)

data World = World Int Int [Int] [Spider] [Plant] [Bug] [LogEntry] deriving(Show, Eq, Generic, NFData)

data Spider = Spider Pos Int | SpiderAttack Pos Pos Int deriving(Show, Eq, Ord, Generic, NFData)

type RunnerM s w a = StateT s (Writer w) a

data LogEntry = PlantsAdded | BugMutated Pos | BugAte Pos Int | SpiderAteBug Pos | SpiderAttacking Pos | SpiderBounced Pos | BugBounced Pos | SpiderStarved Pos deriving(Show, Eq, Generic, NFData)

data Plant = Plant Pos Int deriving(Show, Eq, Ord, Generic, NFData)
