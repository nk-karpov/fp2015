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

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy (x:xs)) (TVar t) = if t == fst x then snd x else appSubsTy (SubsTy xs) (TVar t) 
appSubsTy x (a :-> b) = appSubsTy x a :-> appSubsTy x b


appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv s (Env e) = Env $ map (\x -> (fst x, appSubsTy s (snd x))) e
