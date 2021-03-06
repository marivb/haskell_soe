module Region (Region (Shape, Translate, Scale, Complement, Union, Intersect, Empty),
               Coordinate, Vector,
               containsS, containsR,
               module Shape
  ) where

import Shape

infixr 5 `Union`
infixr 6 `Intersect`

type Vector = (Float, Float)

data Region = Shape Shape
            | Translate Vector Region
            | Scale Vector Region
            | Complement Region
            | Region `Union` Region
            | Region `Intersect` Region
            | Empty
            deriving Show

type Coordinate = (Float, Float)

containsS :: Shape -> Coordinate -> Bool
containsS (Rectangle s1 s2) (x, y)
    = let t1 = s1/2
          t2 = s2/2
      in -t1 <= x && x <= t1 && -t2 <= y && y <= t2
containsS (Ellipse r1 r2) (x, y)
    = (x/r1)^2 + (y/r2)^2 <= 1
containsS (Polygon pts) p
    = let leftOfList = map (isLeftOf p) (zip pts (tail pts ++ [head pts]))
      in and leftOfList
containsS (RtTriangle s1 s2) p
    = let vs = if signum s1 == signum s2 then [(0,0), (s1,0), (0,s2)]
                                         else [(0,0), (0,s2), (s1,0)]
      in containsS (Polygon vs) p

type Ray = (Coordinate, Coordinate)

isLeftOf :: Coordinate -> Ray -> Bool
isLeftOf (px, py) ((ax, ay), (bx, by))
    = let (s, t) = (px - ax, py - ay)
          (u, v) = (px - bx, py - by)
      in s * v >= t * u

containsR :: Region -> Coordinate -> Bool
(Shape s) `containsR` p = s `containsS` p
(Translate (u, v) r) `containsR` (x, y) = r `containsR` (x - u, y - v)
(Scale (u, v) r) `containsR` (x, y) = r `containsR` (x/u, y/v)
(Complement r) `containsR` p = not (r `containsR` p)
(r1 `Union` r2) `containsR` p = r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR` p = r1 `containsR` p && r2 `containsR` p
Empty `containsR` p = False
