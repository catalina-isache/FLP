module Eval where

import Exp
import Data.List (union, delete)

vars :: Exp -> [IndexedVar]
vars (X x) = [x]
vars (App exp1 exp2) = vars exp1 `union` vars exp2
vars (Lam var exp) = [var] `union` vars exp

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

freeVars :: Exp -> [IndexedVar]
freeVars (X x) = [x]
freeVars (App exp1 exp2) = freeVars exp1 `union` freeVars exp2
freeVars (Lam var exp) = delete var (freeVars exp)

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

occursFree :: IndexedVar -> Exp -> Bool
var `occursFree` exp = var `elem` freeVars exp

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar var existingVars = var { ivCount = maxCount + 1 }
  where
    counts = [ivCount v | v <- var : existingVars, ivName v == ivName var]
    maxCount = maximum counts

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement = go
  where
    go (X var)
      | var == toReplace = replacement
      | otherwise = X var
    go (App exp1 exp2) = App (go exp1) (go exp2)
    go (Lam var exp)
      | var == toReplace = Lam var exp
      | var `occursFree` replacement =
          let fresh = freshVar var (vars exp `union` vars replacement)
           in Lam fresh (go (renameVar var fresh exp))
      | otherwise = Lam var (go exp)

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement = go
  where
    go (X var)
      | var == toReplace = replacement
      | otherwise = X var
    go (App exp1 exp2) = App (go exp1) (go exp2)
    go (Lam var exp)
      | var == toReplace = Lam var exp
      | var `occursFree` replacement =
          let fresh = freshVar var (vars exp `union` vars replacement)
           in Lam fresh (go (renameVar var fresh exp))
      | otherwise = Lam var (go exp)

normalize :: Exp -> Exp
normalize exp = maybe exp normalize (step exp)
  where
    step (X _) = Nothing
    step (Lam var exp) = Lam var <$> step exp
    step (App (Lam var exp1) exp2) = Just (substitute var exp2 exp1)
    step (App exp1 exp2) =
      case step exp1 of
        Nothing -> App exp1 <$> step exp2
        Just exp1' -> Just (App exp1' exp2)

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})
