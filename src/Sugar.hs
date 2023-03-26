
module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar (Var name) = IndexedVar name 0

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar name 0) = Var name
sugarVar (IndexedVar name count) = Var (name ++ "_" ++ show count)

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}

consExp, nilExp, zeroExp, succExp, fixExp :: Exp
consExp = X (makeIndexedVar ":")  -- : :: a -> List a -> List a  list constructor
nilExp = X (makeIndexedVar "Nil") -- Nil :: List a               empty list
zeroExp = X (makeIndexedVar "Z")  -- Z :: Natural                zero
succExp = X (makeIndexedVar "S")  -- S :: Natural -> Natural     successor
fixExp = X (makeIndexedVar "fix") -- fix :: (a -> a) -> a        fixpoint fn.

desugarExp :: ComplexExp -> Exp
desugarExp (CApp e1 e2) = App (desugarExp e1) (desugarExp e2)
desugarExp (CLam v e) = Lam (desugarVar v) (desugarExp e)
desugarExp (CX v) = X (desugarVar v)
desugarExp (List []) = nilExp
desugarExp (List (x:xs)) = App (App consExp (desugarExp x)) (desugarExp (List xs))
desugarExp (Nat 0) = zeroExp
desugarExp (Nat n) = App succExp (desugarExp (Nat (n - 1)))
desugarExp (Let v ex e) = App (Lam (desugarVar v) (desugarExp e)) (desugarExp ex)
desugarExp (LetRec v ex e) = App (App fixExp (Lam (desugarVar v) (desugarExp e))) (desugarExp ex)

-- >>> desugarExp (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

-- >>> desugarExp (List [CX (Var "y"), CX (Var "x")])
-- App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))) (App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "Nil", ivCount = 0})))

-- >>> desugarExp (Let (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0}))

-- >>> desugarExp (LetRec (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (App (X (IndexedVar {ivName = "fix", ivCount = 0})) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))))

sugarExp :: Exp -> ComplexExp
sugarExp (App e1 e2) = CApp (sugarExp e1) (sugarExp e2)
sugarExp (Lam v e) = CLam (sugarVar v) (sugarExp e)
sugarExp (X v) = CX (sugarVar v)
sugarExp (Nat 0) = Nat 0
sugarExp (Nat n) = succExp `CApp` sugarExp (Nat (n - 1))
sugarExp e = List (map (CX . sugarVar) (sugarExpList e))
  where
    sugarExpList :: Exp -> [Var]
    sugarExpList (App e1 e2) = sugarExpList e1 ++ sugarExpList e2
    sugarExpList (Lam v e) = v : sugarExpList e
    sugarExpList (X _) = []
    sugarExpList _ = error "Invalid expression"


-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))

-- >>> sugarExp (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 

