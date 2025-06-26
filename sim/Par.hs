module Par where

import Control.Parallel
import Control.DeepSeq

par3 :: (NFData a, NFData b, NFData c) => a -> b -> c -> d -> d
par3 a b c d = do
    let a' = seq a a 
        b' = seq b b
        c' = seq c c
    a' `par` (b' `par` (c' `par` d))
