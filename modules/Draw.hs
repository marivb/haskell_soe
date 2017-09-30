module Draw (inchToPixel, pixelToInch, intToFloat,
             xWin, yWin, trans, shapeToGraphic, spaceClose
  ) where

import Shape
import Graphics.SOE

inchToPixel :: Float -> Int
inchToPixel x = round (100 * x)

pixelToInch :: Int -> Float
pixelToInch n = intToFloat n/100

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

xWin, yWin :: Int
xWin = 600
yWin = 500

trans :: Vertex -> Point
trans (x, y) = (midWinX + inchToPixel x, midWinY + inchToPixel y)

midWinX, midWinY :: Int
midWinX = div xWin 2
midWinY = div yWin 2

transList :: [Vertex] -> [Point]
transList [] = []
transList (p:ps) = trans p : transList ps

shapeToGraphic :: Shape -> Graphic
shapeToGraphic (Rectangle s1 s2) =
  let x = s1 / 2
      y = s2 / 2
  in polygon (transList [(-x, -y), (-x, y),
                         (x, y), (x, -y)])

shapeToGraphic (Ellipse r1 r2) = ellipse (trans (-r1, -r2)) (trans (r1, r2))
shapeToGraphic (RtTriangle s1 s2) = polygon (transList [(0, 0), (s1, 0), (0, s2)])
shapeToGraphic (Polygon vts) = polygon (transList vts)

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w
