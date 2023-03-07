module Printing (showExp) where

import Exp
import Data.List (intercalate)

-- Helper function to show variables
showVar :: Var -> String
showVar (Var v) = v

-- Recursive function to show ComplexExp
showExp :: ComplexExp -> String
showExp (CX var) = showVar var
showExp (CApp exp1 exp2) = "(" ++ showExp exp1 ++ " " ++ showExp exp2 ++ ")"
showExp (CLam var exp) = "\\" ++ showVar var ++ " -> " ++ showExp exp
showExp (Let var exp1 exp2) = "let " ++ showVar var ++ " := " ++ showExp exp1 ++ " in " ++ showExp exp2
showExp (LetRec var exp1 exp2) = "letrec " ++ showVar var ++ " := " ++ showExp exp1 ++ " in " ++ showExp exp2
showExp (List exps) = "[" ++ intercalate "," (map showExp exps) ++ "]"
showExp (Nat n) = show n
