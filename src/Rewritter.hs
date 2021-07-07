module Rewritter where

import Expression
import Evaluator

simplifier :: Exp a -> Exp a
simplifier = undefined

zeroProd :: Exp a -> Exp a
zeroProd (Node "*" xs) 
  | Node "0" [] `elem` xs = Node "0" [] 
  | otherwise = Node "*" xs
zeroProd (Node s xs) = Node s xs

traverseTree :: Exp a -> Exp a
traverseTree (Node s []) = Node s [] 
traverseTree (Node s xs) =
   zeroProd $ Node s (map traverseTree xs)

