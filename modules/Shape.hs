module Shape (
  Shape (Rectangle, Ellipse, RtTriangle, Polygon),
  Radius, Side, Vertex,
  square, circle, distance, area
  ) where

type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show

square :: Side -> Shape
square s = Rectangle s s

circle :: Radius -> Shape
circle r = Ellipse r r

area :: Shape -> Float
area (Rectangle s1 s2)  = s1 * s2
area (RtTriangle s1 s2) = s1 * s2 / 2
area (Ellipse r1 r2)    = pi * r1 * r2

area (Polygon (v1:vs)) = polygonArea vs
    where polygonArea             :: [Vertex] -> Float
          polygonArea (v2:v3:vs') = triangleArea v1 v2 v3 +
                                    polygonArea (v3:vs')
          polygonArea _           = 0

triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea v1 v2 v3 = let a = distance v1 v2
                            b = distance v2 v3
                            c = distance v3 v1
                            s = 0.5 * (a + b + c)
                        in sqrt (s * (s - a) * (s - b) * (s - c))

distance :: Vertex -> Vertex -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
