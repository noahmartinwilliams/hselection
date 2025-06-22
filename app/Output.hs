module Output where

import UI.HSCurses.Curses
import Commands
import Control.Concurrent

obey :: Window -> [Command] -> Int -> Int -> IO ()
obey _ [] _ _ = return ()
obey w (RefreshScr : rest) c r = do
    wRefresh w
    obey w rest c r

obey w (ClrScr : rest) c r = do
    wclear w
    obey w rest c r

obey w (Wait i : rest) c r = do
    threadDelay i
    obey w rest c r 

obey w (DrawSpider (col, row) : rest) c r = do
    move row col
    wAddStr w "s"
    obey w rest c r

obey w (DrawPlant (col, row) : rest) c r = do
    move row col
    wAddStr w "p"
    obey w rest c r

obey w (DrawBug (col, row) : rest) c r = do
    move row col
    wAddStr w "b"
    obey w rest c r

obey w (DrawStr str (col, row) : rest) c r = do
    move row col
    wAddStr w str
    obey w rest c r
