module Main where

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch t1 t2) = Branch (mapTree f t1) (mapTree f t2)

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch t1 t2) = fringe t1 ++ fringe t2
