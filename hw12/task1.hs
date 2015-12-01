infixl 2 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = 
  Var Symb 
  | Expr :@ Expr
  | Lam Symb Expr
     deriving (Eq,Show)

-- Тип
data Type = 
  TVar Symb 
  | Type :-> Type
    deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

freeVars :: Expr -> [Symb] 
freeVars (Var x) = [x]
freeVars (a :@ b) = (freeVars a) ++ [v | v <- freeVars b, False <- [v `elem` freeVars a]]
freeVars (Lam x e) = [v | v <- freeVars e, False <- [v == x]] 

freeTVars :: Type -> [Symb]
freeTVars (TVar t) = [t]
freeTVars (a :-> b) = freeTVars a ++ [v | v <- freeTVars b, False <- [v `elem` freeTVars a]]

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) v e = if (v, e) `elem` env then Env env else Env (env ++ [(v, e)])

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = unique [] [t | (_, ts) <- env, t <- freeTVars ts]

unique :: Eq a => [a] -> [a] -> [a]
unique xs [] = xs
unique xs (y:ys) = (if (y `elem` xs) then (unique xs) else unique (y:xs)) ys

