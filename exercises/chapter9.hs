module Chapter9Exercises where

simple x y z = x * (y + z)

-- | 9.4
-- Examples:
-- >>> applyEach [simple 2 2, (+3)] 5
-- [14,8]
applyEach :: [(a -> b)] -> a -> [b]
applyEach list value = map (\x -> x value) list

-- | 9.5
-- Examples:
-- >>> applyAll [simple 2 2, (+3)] 5
-- 20
applyAll :: [(a -> a)] -> a -> a
applyAll list value = foldr (\f x -> f x) value list

-- | 9.7
-- Examples:
-- >>> twice (+1) 2
-- 4
-- >>> (twice twice) (+1) 2
-- 6
twice :: (a -> a) -> a -> a
twice f v = f (f v)

-- | 9.9
fix :: (a -> a) -> a
fix f = f (fix f)

-- | remainder
-- Examples
-- >>> remainder 10 3
-- 1
remainder :: Integer -> Integer -> Integer
remainder a b = if a < b then a
                else remainder (a - b) b

-- | 9.10
-- Examples:
-- >>> ex10 [9, 5]
-- [5.0,3.0]
ex10 :: [Float] -> [Float]
ex10 = map ((/2) . (+1))
