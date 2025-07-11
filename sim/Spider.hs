module Spider(getSpiderPos, getSpiderEnergy, spiderIsAttacking, decSpiderEnergy) where

import Pos 
import Control.Monad.Writer
import HSelect.Types

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
decSpiderEnergy (Spider p e) a | e == 1 = do
    tell [a]
    return (Spider p 0)
decSpiderEnergy (Spider p energy) _ = return (Spider p (energy - 1))
decSpiderEnergy (SpiderAttack p1 p2 e) a | e == 1 = do
    tell [a]
    return (SpiderAttack p1 p2 0)
decSpiderEnergy (SpiderAttack p1 p2 energy) _ = return (SpiderAttack p1 p2 (energy - 1))

