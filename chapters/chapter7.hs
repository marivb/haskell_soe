module Main where

-- Trees
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch t1 t2) = Branch (mapTree f t1) (mapTree f t2)

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch t1 t2) = fringe t1 ++ fringe t2

treeSize :: Tree a -> Integer
treeSize (Leaf f) = 0
treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

treeHeight :: Tree a -> Integer
treeHeight (Leaf f) = 0
treeHeight (Branch t1 t2) = 1 + max (treeHeight t1) (treeHeight t2)

-- Expressions
data Expr = C Float
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2

e = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)
main = do putStrLn (show (evaluate e))
