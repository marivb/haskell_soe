module Main where

import SOE

main = runGraphics (
    do w <- openWindow "Meu Primeiro Programa Grafico" (300, 300)
       drawInWindow w (text (100, 200) "Ola Mundo Grafico")
       spaceClose w
    )

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w


