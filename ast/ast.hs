module Ast where

data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr
    | Literal Float

eval :: Expr -> Float
eval (Literal x)        = x
eval (Plus expr1 expr2) = eval expr1 + eval expr2
eval (Minus expr1 expr2) = eval expr1 - eval expr2
eval (Times expr1 expr2) = eval expr1 * eval expr2
eval (Div expr1 expr2)   = eval expr1 / eval expr2

-- Should eval to "5.0"
test1 = Plus (Literal 3.0) (Literal 2.0)

-- Should eval to "3.5"
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

-- Should eval to "15.5"
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))

