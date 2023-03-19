module Main where

import           System.IO

type Result = (Expr, Int)

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = ts ++ map (x:) ts where
  ts = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs): [(x:ls,rs) | (ls,rs) <- split xs]

data Op = Add | Sub | Mul | Div
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n) = show n
  show (App op lexpr rexpr) = brak lexpr ++ show op ++ brak rexpr where
    brak (Val n) = show n
    brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)             = [n]
values (App _ lexpr rexpr) = values lexpr ++ values rexpr

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op lexpr rexpr) = [apply op x y | x <- eval lexpr,
                             y <- eval rexpr,
                             valid op x y]


exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

combine :: Expr -> Expr -> [Expr]
combine left right = [App op left right | op <- ops]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

solutionsBrute :: [Int]-> Int -> [Expr]
solutionsBrute ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App op l r, apply op x y) | op <- ops, valid op x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

e :: Expr
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter target number: "
  input <- getNum
  putStrLn ( show $ solutions' [1,3,7,10,25,50] input)
