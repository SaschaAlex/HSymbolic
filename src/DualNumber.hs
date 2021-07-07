module DualNumber where

newtype Dual a = Dual (a,a)

instance Show a => Show (Dual a ) where
    show (Dual (a,b)) = show a ++ " + ε" ++ show b

instance Num a => Num (Dual a ) where
  (Dual (a,b)) + (Dual (c,d)) = Dual (a + c, b + d)
  (Dual (a,b)) * (Dual (c,d)) = Dual (a * c,a*d + b*c) 
  negate (Dual (a,b)) = Dual (-a,-b)
  signum (Dual(a,b)) = Dual (signum a,signum b)
  fromInteger n = Dual (fromInteger n , 0)
  abs (Dual(a,b)) = Dual(abs a, b * signum a )

instance (Fractional a) => Fractional (Dual a) where 
  (Dual (u,u')) / (Dual (v,v')) = Dual (u/v, (u'*v - u*v')/ (v*v))
  fromRational q  = Dual (fromRational q , 0)

instance (Floating a) => Floating (Dual a) where
  pi = Dual (pi , 0)
  exp (Dual (u,u')) = Dual (exp u , u' * exp u )
  log (Dual (u,u')) = Dual (log u , u'/ u)
  (Dual (u,u')) ** (Dual (v,v')) = Dual (u**v,(u**v)*( (v*u'/u) + log(u)*v'))
  logBase (Dual (u,u')) (Dual (v,v')) = Dual (logBase u v ,v'/(v*log u) - (u'*log v )/(u*(log u)**2))
  sin (Dual (u,u')) = Dual (sin u , u' * cos u)
  cos (Dual (u,u')) = Dual (cos u , -1*u' * sin u)
  tan (Dual (u,u')) = Dual (tan u , u'/(cos u) ** 2 )
  asin (Dual (u,u')) = Dual (asin u , u' / sqrt (1 - u**2))
  acos (Dual (u,u')) = Dual (acos u , -u' / sqrt (u**4-u**2))
  atan (Dual (u,u')) = Dual (atan u , u' / (1 + u**2))
  sinh (Dual (u,u')) = Dual (sinh u , u'*cosh u )
  cosh (Dual (u,u')) = Dual (cosh u , u'*sinh u )
  tanh (Dual (u,u')) = Dual (tanh u , u'*(1- (tanh u )**2))
  asinh (Dual (u,u')) = Dual (asinh u ,u' / sqrt (u**2 + 1 ) )
  acosh (Dual (u,u')) = Dual (acosh u ,u' / sqrt (u**2 - 1 ))
  atanh (Dual (u,u')) = Dual (atanh u , u'/ (1-u**2) )
