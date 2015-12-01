import Data.Maybe

type Symb = String 
infixl 2 :@
infixl 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

subst :: Symb -> Expr -> Expr -> Expr 
subst v n (Var x) = if x == v then n else Var x
subst v n (a :@ b) = (subst v n a :@ subst v n b)
subst v n (Lam x e) 
  |x == v = Lam v e
  |otherwise = if test x n then subst v n (Lam (x ++ x) (subst x (Var (x ++ x)) e)) else Lam x (subst v n e)

test :: Symb -> Expr -> Bool
test v (Var x) = x == v
test v (a :@ b) = test v a || test v b
test v (Lam x e) = if x /= v then test v e else False

alphaEq :: Expr -> Expr -> Bool

alphaEq (Var x) (Var y) = x == y
alphaEq (a :@ b) (c :@ d) = alphaEq a c && alphaEq b d
alphaEq (Lam x e) (Lam y f) = ((alphaEq e (subst y (Var x) f)))
alphaEq _ _ = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce ((Lam x e) :@ s) = Just (subst x s e)
reduceOnce (Lam x e) = if isJust (reduceOnce e) then Just (Lam x (fromJust (reduceOnce e))) else Nothing 
reduceOnce (a :@ b)
  | isJust (reduceOnce a) = Just (fromJust (reduceOnce a):@ b)
  | isJust (reduceOnce b) = Just (a :@ fromJust (reduceOnce b))
  | otherwise = Nothing
reduceOnce _ = Nothing

nf :: Expr -> Expr
nf e = if isJust (reduceOnce e) then nf (fromJust (reduceOnce e)) else e

infix 1 `betaEq`
betaEq :: Expr -> Expr -> Bool
betaEq a b = (nf a) `alphaEq` (nf b)
