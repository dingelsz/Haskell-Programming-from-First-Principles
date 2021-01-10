data Expr =
  Lit Integer
  | Add Expr Expr

instance Show Expr where
  show (Lit n) = show n
  show (Add e1 e2) = (show e1) ++ " + " ++ (show e2)
  
eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
