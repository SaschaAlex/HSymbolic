module Evaluator where 

import DualNumber
import Expression

evalDerivate :: (Floating a,Fractional a) => (Dual a -> Dual a) -> a -> a
evalDerivate f x = snd t
    where (Dual t ) = f $ Dual (x,1) 
