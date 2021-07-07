module Expression where

import Data.List

data Exp a = Node String [Exp a]
  deriving (Show,Eq)

instance (Num a) => Num (Exp a) where 
  l + r = Node "+" [l , r]
  l * r = Node "*" [l , r]
  negate term = Node "-" [term]
  signum term = Node "signum" [term]
  fromInteger n = Node (show n) []
  abs term = Node "abs" [term]

instance (Fractional a ) => Fractional (Exp a) where 
  l / r = Node "/" [l,r]
  fromRational q = Node (show q) []

instance (Fractional a) => Floating (Exp a) where
  pi = Node "pi" []
  exp term = Node "exp" [term] 
  log term = Node "log" [term]
  sqrt term = Node "sqrt" [term]
  term ** term' = Node "power" [term , term']
  logBase term term' = Node "logbase" [term , term']
  sin term = Node "sin" [term]
  cos term = Node "cos" [term]
  tan term = Node "tan" [term]
  asin term = Node "asin" [term]
  acos term = Node "acos" [term]
  atan term = Node "atan" [term]
  sinh term = Node "sinh" [term]
  cosh term = Node "cosh" [term]
  tanh term = Node "tanh" [term]
  asinh term = Node "asinh" [term]
  acosh term = Node "acosh" [term]
  atanh term = Node "atanh" [term]

toJson :: Exp a -> String 
toJson (Node string []) = "\""++string ++ "\""
toJson (Node string xs) = 
  "{ \"" ++ string ++ "\" : " ++ list ++  "}"
  where list = "[" ++ intercalate "," (map toJson xs)  ++ "]"
