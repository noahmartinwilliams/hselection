module Spider(Spider(..), getSpiderPos) where

import Pos 
data Spider = Spider Pos Int deriving(Show, Eq, Ord)

getSpiderPos :: Spider -> Pos
getSpiderPos (Spider p _) = p
