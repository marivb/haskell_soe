module Picture (Picture (Region, Over, EmptyPicture),
                Color (Black, Blue, Green, Cyan,
                       Red, Magenta, Yellow, White),
                regionToGRegion, shapeToGRegion,
                drawRegionInWindow, drawPicture, draw, draw2,
                spaceClose, module Region
               ) where

import Draw
import Region
--import Graphics.SOE hiding (Region)
--import qualified Graphics.SOE as G (Region)
import SOE hiding (Region)
import qualified SOE as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPicture
             deriving Show

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r = drawInWindow w (withColor c
                                            (drawRegion (regionToGRegion r)))

drawPicture :: Window -> Picture -> IO ()
drawPicture w (Region c r)   = drawRegionInWindow w c r
drawPicture w (p1 `Over` p2) = do drawPicture w p2; drawPicture w p1
drawPicture w EmptyPicture   = return ()

regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0, 0) (1, 1) r

regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s)
  = shapeToGRegion loc sca s
regToGReg loc (sx, sy) (Scale (u, v) r)
  = regToGReg loc (sx*u, sy*v) r
regToGReg (lx, ly) (sx, sy) (Translate (u, v) r)
  = regToGReg (lx + u*sx, ly + v*sy) (sx, sy) r
regToGReg loc sca Empty
  = createRectangle (0,0) (0,0)
regToGReg loc sca (r1 `Union` r2)
  = primGReg loc sca r1 r2 orRegion
regToGReg loc sca (r1 `Intersect` r2)
  = primGReg loc sca r1 r2 andRegion
regToGReg loc sca (Complement r)
  = primGReg loc sca winRect r diffRegion

primGReg :: Vector -> Vector -> Region -> Region -> (G.Region -> G.Region -> G.Region) -> G.Region
primGReg loc sca r1 r2 op
  = let gr1 = regToGReg loc sca r1
        gr2 = regToGReg loc sca r2
    in op gr1 gr2

winRect :: Region
winRect = Shape (Rectangle (pixelToInch xWin) (pixelToInch yWin))

shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx, ly) (sx, sy) s
  = case s of
         (Rectangle s1 s2)
            -> createRectangle (trans (-s1/2, -s2/2))
                               (trans (s1/2, s2/2))
         (Ellipse r1 r2)
            -> createEllipse (trans (-r1, -r2)) (trans (r1, r2))
         (Polygon vs)
            -> createPolygon (map trans vs)
         (RtTriangle s1 s2)
            -> createPolygon (map trans [(0,0), (s1,0), (0,s2)])
    where trans :: Vertex -> Point
          trans (x, y) = (xWin2 + inchToPixel (lx + x * sx),
                          yWin2 - inchToPixel (ly + y * sy))

xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

draw :: String -> Picture -> IO ()
draw s p = runGraphics $
           do w <- openWindow s (xWin, yWin)
              drawPicture w p
              spaceClose w

pictToList :: Picture -> [(Color, Region)]
pictToList EmptyPicture = []
pictToList (Region c r) = [(c, r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2

adjust :: [(Color, Region)] -> Coordinate -> (Maybe (Color, Region), [(Color, Region)])
adjust regs p = case (break (\(_,r) -> r `containsR` p) regs) of
                     (top, hit:rest) -> (Just hit, top ++ rest)
                     (_, [])         -> (Nothing, regs)

loop :: Window -> [(Color, Region)] -> IO ()
loop w regs =
  do clearWindow w
     sequence_ [drawRegionInWindow w c r | (c,r) <- reverse regs]
     (x, y) <- getLBP w
     case (adjust regs (pixelToInch (x - xWin2),
                        pixelToInch (yWin2 - y))) of
          (Nothing, _)        -> closeWindow w
          (Just hit, newRegs) -> loop w (hit:newRegs)

draw2 :: String -> Picture -> IO ()
draw2 s p = runGraphics $
            do w <- openWindow s (xWin, yWin)
               loop w (pictToList p)
