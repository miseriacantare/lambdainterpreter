type Name = String

data Term =  Var Name
           | Con Int
           | Log Bool
           | Not Term
           | Add Term Term
           | Lam Name Term
           | App Term Term

data Value =  Wrong
            | Num Int
            | Bo Bool
            | Fun (Value -> E Value)

type Environment = [(Name,Value)]

data E a = Success a | Error String

unitE a = Success a
errorE s = Error s

(Success a) `bindE` k = k a
(Error s) `bindE` k = Error s

showE (Success a) = "Success: " ++ showval a
showE (Error s) = "Error: " ++ s

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Bo a) = show a
showval (Fun f) = "<function>"

interp :: Term -> Environment -> E Value
interp (Var x) e = looker x e
interp (Con i) e = unitE (Num i)
interp (Log a) e = unitE (Bo a)
interp (Add x y) e = interp x e `bindE` (\a ->
                     interp y e `bindE` (\b ->
                     add a b))
interp (Lam x v) e = unitE (Fun (\a -> interp v ((x,a):e)))
interp (App u v) e = interp u e `bindE` (\f ->
                     interp v e `bindE` (\a ->
                     apply f a))

looker :: Name -> Environment -> E Value
looker x [] = errorE ("Unbound variable: " ++ x)
looker x ((y,b):e) = if x == y then unitE b else looker x e

add :: Value -> Value -> E Value
add (Num i) (Num j) = unitE (Num (i+j))
add a b = errorE ("Invalid type. " ++ showval a ++ " or " ++ showval b ++ " is not a number.")

apply :: Value -> Value -> E Value
apply (Fun k) a = k a
apply f a = errorE ("Invalid type." ++ showval f ++ " should be a function.")

test :: Term -> String
test t = showE (interp t [])
