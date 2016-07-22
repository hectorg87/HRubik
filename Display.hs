module Display (display,reshape,idle, Inst(..), CubeState(..), goalState, randomMoves) where

import Graphics.UI.GLUT hiding (index, R)
import Data.IORef
import Control.Monad
import RubikCube
import GI
import Sprites

display :: CubeState -> [TextureObject] -> DisplayCallback
display state tex = do
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity   -- clear the matrix
   lookAt (Vertex3 6 5 8) (Vertex3 0 0 0) (Vector3 0 1 0)

   c <- readIORef $ cube state

   cubeAction state tex

   swapBuffers
--   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let near   = 1
       far    = 80
       fov    = 90
       ang    = (fov*pi)/(360 :: GLdouble)
       top    = near / ( cos(ang) / sin(ang) )
       aspect = fromIntegral(w)/fromIntegral(h)
       right  = top*aspect
   frustum (-right) right (-top) top near far
   matrixMode $= Modelview 0

idle = do
  postRedisplay Nothing

cubeAction:: CubeState -> [TextureObject] -> IO ()
cubeAction state textures = do
  rCube <- readIORef $ cube state
  let ins   = inst state
  i <- readIORef ins
  print i
  when (head i == NO) $ do
    c <- readIORef $ cube state
    paintCube textures c
  when ( elem (head i) [TF,TF',TU,TU',TR,TR',R, R',MF,MF',L,L',F,F',MR,MR',B,B',U,U',MU,MU',D,D'] ) $ do
    let ro = rot state
    r <- readIORef ro
    if r < 90 then do
        animation textures (head i) state r
        writeIORef (rot state) (r+5)
      else do
        let lista = insts state
        l <- readIORef lista
        animation textures (head i) state 90
        nextMove (head i) state
        if(null (tail i)) then writeIORef ins ([NO])
          else writeIORef ins ((tail i))
        writeIORef lista (inverseMove (head i):l)
        writeIORef ro 0.0

animation:: [TextureObject] -> Inst -> CubeState -> GLfloat -> IO ()
animation textures F state r = do
  c <- readIORef $ cube state
  paintRow2 textures c
  paintRow1 textures c
  animate c paintRow0 textures (negate r) (Vector3 0 0 1)
animation textures F' state r = do
  c <- readIORef $ cube state
  paintRow2 textures c
  paintRow1 textures c
  animate c paintRow0 textures r (Vector3 0 0 1)
animation textures MR state r = do
  c <- readIORef $ cube state
  paintRow2 textures c
  paintRow1 textures c
  animate c paintRow0 textures (negate r) (Vector3 0 0 1)
animation textures MR' state r = do
  c <- readIORef $ cube state
  paintRow2 textures c
  paintRow1 textures c
  animate c paintRow0 textures (negate r) (Vector3 0 0 1)
animation textures B state r = do
  c <- readIORef $ cube state
  animate c paintRow2 textures r (Vector3 0 0 1)
  paintRow1 textures c
  paintRow0 textures c
animation textures B' state r = do
  c <- readIORef $ cube state
  animate c paintRow2 textures (negate r) (Vector3 0 0 1)
  paintRow1 textures c
  paintRow0 textures c
animation textures L' state r = do
  c <- readIORef $ cube state
  animate c paintColum2 textures (negate r) (Vector3 1 0 0)
  paintColum1 textures c
  paintColum0 textures c
animation textures L state r = do
  c <- readIORef $ cube state
  animate c paintColum2 textures r (Vector3 1 0 0)
  paintColum1 textures c
  paintColum0 textures c
animation textures MF' state r = do
  c <- readIORef $ cube state
  paintColum2 textures c
  animate c paintColum1 textures (negate r) (Vector3 0 0 1)
  paintColum0 textures c
animation textures MF state r = do
  c <- readIORef $ cube state
  paintColum2 textures c
  animate c paintColum1 textures (negate r) (Vector3 0 0 1)
  paintColum0 textures c
animation textures R' state r = do
  c <- readIORef $ cube state
  paintColum2 textures c
  paintColum1 textures c
  animate c paintColum0 textures r (Vector3 1 0 0)
animation textures R state r = do
  c <- readIORef $ cube state
  paintColum2 textures c
  paintColum1 textures c
  animate c paintColum0 textures (negate r) (Vector3 1 0 0)
animation textures U state r = do
  c <- readIORef $ cube state
  paintLayer2 textures c
  paintLayer1 textures c
  animate c paintLayer0 textures (negate r) (Vector3 0 1 0)
animation textures U' state r = do
  c <- readIORef $ cube state
  paintLayer2 textures c
  paintLayer1 textures c
  animate c paintLayer0 textures r (Vector3 0 1 0)
animation textures MU state r = do
  c <- readIORef $ cube state
  paintLayer2 textures c
  animate c paintLayer1 textures r (Vector3 0 1 0)
  paintLayer0 textures c
animation textures MU' state r = do
  c <- readIORef $ cube state
  paintLayer2 textures c
  animate c paintLayer1 textures r (Vector3 0 1 0)
  paintLayer0 textures c
animation textures D state r = do
  c <- readIORef $ cube state
  animate c paintLayer2 textures r (Vector3 0 1 0)
  paintLayer1 textures c
  paintLayer0 textures c
animation textures D' state r = do
  c <- readIORef $ cube state
  animate c paintLayer2 textures (negate r) (Vector3 0 1 0)
  paintLayer1 textures c
  paintLayer0 textures c
animation textures TF state r = do
  c <- readIORef $ cube state
  animate c paintCube textures (negate r) (Vector3 0 0 1) 
animation textures TF' state r = do
  c <- readIORef $ cube state
  animate c paintCube textures r (Vector3 0 0 1) 
animation textures TU state r = do
  c <- readIORef $ cube state
  animate c paintCube textures (negate r) (Vector3 0 1 0) 
animation textures TU' state r = do
  c <- readIORef $ cube state
  animate c paintCube textures r (Vector3 0 1 0) 
animation textures TR state r = do
  c <- readIORef $ cube state
  animate c paintCube textures (negate r) (Vector3 1 0 0) 
animation textures TR' state r = do
  c <- readIORef $ cube state
  animate c paintCube textures r (Vector3 1 0 0) 
animation _ _ _ _ = return ()

animate c f t r' vec =
  preservingMatrix $ do
    rotate r' vec
    f t c

