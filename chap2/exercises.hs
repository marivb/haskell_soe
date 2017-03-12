module Exercise2 where

import Shape

-- 2.1
rectangle :: Side -> Shape
rectangle s = Polygon [(0,0), (0,s), (s,s), (s, 0)]

rtTriangle :: Side -> Side -> Shape
rtTriangle s1 s2 = Polygon [(0,0), (0,s1), (s2,0)]

-- 2.2
-- regularPolygon :: Int -> Side -> Shape

-- 2.4
-- convex :: Shape -> Bool
-- http://math.stackexchange.com/questions/1743995/determine-whether-a-polygon-is-convex-based-on-its-vertices

-- 2.5
-- area from trapezoids
