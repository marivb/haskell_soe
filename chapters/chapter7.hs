module Chapter7 where

-- Trees
-- $setup
-- >>> let tree = Branch (Branch (Leaf 2) (Leaf 1)) (Leaf 10)

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

-- | Maps values in a tree
--
-- Examples:
--
-- >>> mapTree (* 2) tree
-- Branch (Branch (Leaf 4) (Leaf 2)) (Leaf 20)
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch t1 t2) = Branch (mapTree f t1) (mapTree f t2)

-- | Flattens leaf values into list
--
-- Examples:
--
-- >>> fringe tree
-- [2,1,10]
fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch t1 t2) = fringe t1 ++ fringe t2

-- | Calculates number of leafs in a tree
--
-- Examples:
--
-- >>> treeSize tree
-- 3
treeSize :: Tree a -> Integer
treeSize (Leaf f) = 1
treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

-- | Calculates tree height
--
-- Examples:
--
-- >>> treeHeight tree
-- 2
treeHeight :: Tree a -> Integer
treeHeight (Leaf f) = 0
treeHeight (Branch t1 t2) = 1 + max (treeHeight t1) (treeHeight t2)

-- | 7.1 redefine fringe, treeSize and treeHeight with foldTree
-- Examples:
--
-- Fringe:
-- >>> foldTree (: []) (++) tree
-- [2,1,10]
--
-- Tree size:
-- >>> foldTree (\x -> 1) (+) tree
-- 3
--
-- Tree height:
-- >>> foldTree (\x -> 0) (\h1 h2 -> 1 + (max h1 h2)) tree
-- 2
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree leafFunc branchFunc (Leaf x) = leafFunc x
foldTree leafFunc branchFunc (Branch t1 t2) =
  branchFunc (foldTree leafFunc branchFunc t1)
             (foldTree leafFunc branchFunc t2)

-- Expressions

data Expr = C Float
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
          | Let String Expr Expr
          | V String

-- | Evaluates an arithmetic expression
--
-- Examples:
--
-- >>> let e = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)
-- >>> evaluate e
-- 42.0
--
-- evaluate (Let "x" (C 5) (V "x" :+ V "x"))
-- 10
evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2
