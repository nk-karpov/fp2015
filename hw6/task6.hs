import Data.Maybe
type Symb = String 

infixl 2 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
              deriving Eq

instance Show Expr where
  showsPrec _ = sE

sE :: Expr -> ShowS

sS = showString 
sC = showChar
sE (Var x) = sS x
sE (a :@ b) = sC '('.sE a.sC ' '.sE b.sC ')'              
sE (Lam v e) = sS "(\\".sS v.sS "->".sE e.sC ')'

instance Read Expr where
  readsPrec _ = readsExpr

readsExpr :: ReadS Expr
readsExpr s = [(fromJust e, t) | (e, t) <- readsFun Nothing s]

readsLam :: ReadS Expr
readsLam s = [(Lam v e, t3) | (v, t1) <- lex s, 
                              (c, t2) <- lex t1, 
                              (e, t3) <- if c == "->" then readsExpr t2 else (if c == "\\" then readsLam t2 else readsLam t1)
            ]

readsFun :: Maybe Expr -> ReadS (Maybe Expr)
readsFun e s = [(e', t) | (l, t') <- lex s, 
                          (e', t) <- readsFun' e (l ++ t')
                ]

readsFun' :: Maybe Expr -> ReadS (Maybe Expr)
readsFun' e ('(':s) = [(e'', t3) | (e', t1) <- readsFun Nothing s,
                                  (")", t2) <- lex t1,
                                  (e'', t3) <- readsFun (app e e') t2
                       ]
readsFun' e ('\\':s) = [(app e (Just e1), t1) | (e1, t1) <- readsLam s]
readsFun' e "" = [(e,"")]
readsFun' e (')':s) = [(e, ')':s)]
readsFun' e s = [(e', t2) | (x, t1) <- lex s,
                            (e', t2) <- readsFun (app e (Just (Var x))) t1
                ]

app :: Maybe Expr -> Maybe Expr -> Maybe Expr
app Nothing Nothing = Nothing
app (Just a) Nothing = Just a
app Nothing (Just b) = Just b
app (Just a) (Just b) = Just (a :@ b)
