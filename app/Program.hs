module Program where
import Prelude


data Definition = Definition
  { defHead :: Var,
    defArgs :: [Var],
    defBody :: ComplexExp
  }
  deriving (Show)

definition :: Parser Definition
definition = do
  defHead <- var
  defArgs <- many var
  reservedOp ":="
  defBody <- expr
  return (Definition defHead defArgs defBody)


program :: Parser [Definition]
program = semiSep1 definition

definitionExp :: Definition -> ComplexExp
definitionExp (Definition defHead defArgs defBody) = foldr CLam (desugarExp defBody) (map desugarVar defArgs)

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv pgm = Map.fromList [(desugarVar (defHead def), definitionExp def) | def <- pgm]

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env (EVar var) =
  case Map.lookup var env of
    Just exp -> exp
    Nothing -> EVar var
normalizeEnv env (EApp e1 e2) = EApp (normalizeEnv env e1) (normalizeEnv env e2)
normalizeEnv env (ELam var e) = ELam var (normalizeEnv env e)
normalizeEnv _ exp = exp