module Main where

import Graphics.SOE

main1 = runGraphics (
    do w <- openWindow "Meu Primeiro Programa Grafico" (300, 300)
       drawInWindow w (text (100, 200) "Ola Mundo Grafico")
       spaceClose w
    )

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w

pic1 = withColor Red (ellipse (150, 150) (300, 200))
pic2 = withColor Blue (polyline [(100, 50), (200, 50), (200, 250), (100, 250), (100, 50)])

main2 = runGraphics (
    do w <- openWindow "figuras" (300, 300)
       drawInWindow w pic1
       drawInWindow w pic2
       spaceClose w
    )

fillTriangle :: Window -> Int -> Int -> Int -> IO ()
fillTriangle w x y size = drawInWindow w (withColor Blue
                                         (polygon [(x, y), (x + size, y), (x, y-size)]))

minSize :: Int
minSize = 8

sierpinskiTriangle :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTriangle w x y size =
  if size <= minSize
     then fillTriangle w x y size
     else let size2 = size `div` 2
          in do sierpinskiTriangle w x y size2
                sierpinskiTriangle w x (y-size2) size2
                sierpinskiTriangle w (x+size2) y size2

main = runGraphics (
    do w <- openWindow "Sierpinski Triangle" (400, 400)
       sierpinskiTriangle w 50 300 256
       spaceClose w
    )
