module Main (main) where

import UI.HSCurses.CursesHelper
import UI.HSCurses.Curses
import Output
import System.Random
import Run
import Control.Monad.State
import Control.Monad.Writer
import Control.Parallel.Strategies
import GHC.Conc
import World

main :: IO ()
main = do
    start
    (rows, cols) <- scrSize
    --let (rows, cols) = (25, 128)
    gi <- getStdGen
    let randInts = randoms gi :: [Int]
        dworld = defaultWorld randInts ( 2 * (div cols 3)) rows -- use 2/3 to keep the creatures from crawling over the log on the right of the screen.
        runner = run 3.0
        written = runStateT runner dworld
        ((commands, _), _) = runWriter written
        commands' = (take (16*1024) commands) `using` (parBuffer numCapabilities rdeepseq)
    --putStrLn (foldr (++) "" (map (\x -> (show x ) ++ "\n")  commands'))
    obey stdScr commands' cols rows  -- Take so that the program can eventually stop and we can use threadscope to figure out if it used enough cores.
    end
