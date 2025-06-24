module Spider(Spider(..), randSpiders, getSpiderPos, getSpiderEnergy, spiderIsAttacking, decSpiderEnergy) where

import Pos 
import Control.Monad.Writer

data Spider = Spider Pos Int | SpiderAttack Pos Pos Int deriving(Show, Eq, Ord)

getSpiderEnergy :: Spider -> Int
getSpiderEnergy (Spider _ e) = e
getSpiderEnergy (SpiderAttack _ _ e ) = e

getSpiderPos :: Spider -> Pos
getSpiderPos (Spider p _) = p
getSpiderPos (SpiderAttack p _ _) = p

spiderIsAttacking :: Spider -> Bool
spiderIsAttacking (SpiderAttack _ _ _ ) = True
spiderIsAttacking _ = False

decSpiderEnergy :: Spider -> a -> Writer [a] Spider
decSpiderEnergy (Spider p e) a | e == 11= do
    tell [a]
    return (Spider p 0)
decSpiderEnergy (Spider p energy) _ = return (Spider p (energy - 1))
decSpiderEnergy (SpiderAttack p1 p2 e) a | e == 1= do
    tell [a]
    return (SpiderAttack p1 p2 0)
decSpiderEnergy (SpiderAttack p1 p2 energy) _ = return (SpiderAttack p1 p2 (energy - 1))

randSpiders :: [Int] -> Int -> Int -> Int -> ([Spider], [Int])
randSpiders randLs cols rows n = do
    let xs = take n randLs
        ys = take n (drop n randLs)
        xy = zip xs ys
    (map (\(x, y) -> Spider ((mod (abs x) cols), (mod (abs y) rows)) 50) xy, (drop (2 * n) randLs))
