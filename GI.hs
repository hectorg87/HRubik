module GI where

import Graphics.UI.GLUT hiding (index)
import RubikCube
import Data.Sequence
import Data.Word (Word8)
import Sprites

face :: TextureObject -> IO ()
face tex = do
     let t x y   = texCoord $ TexCoord2 (x :: GLdouble) (y :: GLdouble)
         v x y z = vertex   $ Vertex3 x y z
     textureBinding Texture2D $= Just tex
     renderPrimitive Quads $ do
       color $ Color3 1 1 (1::GLdouble)
       t 0 1 >> v 0.0 2.0 (0 :: GLfloat)
       t 0 0 >> v 0.0 0.0 (0 :: GLfloat)
       t 1 0 >> v 2.0 0.0 (0 :: GLfloat)
       t 1 1 >> v 2.0 2.0 (0 :: GLfloat)
     

       
color3f = color :: Color3 GLfloat -> IO ()

scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()

translatef::GLfloat -> GLfloat -> GLfloat -> IO ()
translatef a b c = translate $ Vector3 a b c

rotatef::GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
rotatef a b c d = rotate a $ Vector3 b c d

--faceTexture:: (Integral b) => [TextureObject] -> b -> TextureObject
faceTexture tex 0 = tex !! 0
faceTexture tex 1 = tex !! 1
faceTexture tex 2 = tex !! 2
faceTexture tex 3 = tex !! 3
faceTexture tex 4 = tex !! 4
faceTexture tex 5 = tex !! 5


paintCube:: [TextureObject] -> RCube -> IO()
paintCube textures (RCube corners edges centers)  = do
  tops textures centers edges corners
--  paintEdges edges
--  paintCenters centers
--  paintCorners corners

{-
fe :: !CFace, -- ^ Front Edge
xy :: !CFace  -- ^ X or Y axis
-}

paintCorners:: [TextureObject] -> Seq Corner -> IO()
paintCorners textures sequence = do
  paintFace textures sequence x 5 (Vector3 (-3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures sequence z 5 (Vector3 (-3) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures sequence y 5 (Vector3 (-3) (3) (-3)) 90 (Vector3 1 0 0)

  paintFace textures sequence z 4 (Vector3 (1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures sequence x 4 (Vector3 (3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures sequence y 4 (Vector3 (1) (3) (-3)) 90 (Vector3 1 0 0)

  paintFace textures sequence y 7 (Vector3 (1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures sequence z 7 (Vector3 (1) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures sequence x 7 (Vector3 (3) (-3) (-1)) 90 (Vector3 0 1 0)

  paintFace textures sequence y 6 (Vector3 (-3) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures sequence z 6 (Vector3 (-3) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures sequence x 6 (Vector3 (-3) (-3) (-1)) 90 (Vector3 0 1 0)

  paintFace textures sequence x 0 (Vector3 (-3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures sequence y 0 (Vector3 (-3) (3) (1)) 90 (Vector3 1 0 0)
  paintFace textures sequence z 0 (Vector3 (-3) (1) (3)) 0 (Vector3 0 0 0)

  paintFace textures sequence x 1 (Vector3 (3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures sequence y 1 (Vector3 (1) (3) (1)) 90 (Vector3 1 0 0)
  paintFace textures sequence z 1 (Vector3 (1) (1) (3)) 0 (Vector3 0 0 0)

  paintFace textures sequence y 2 (Vector3 (1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures sequence x 2 (Vector3 (3) (-3) (3)) 90 (Vector3 0 1 0)
  paintFace textures sequence z 2 (Vector3 (1) (-3) (3)) 0 (Vector3 0 0 0)

  paintFace textures sequence x 3 (Vector3 (-3) (-3) (3)) 90 (Vector3 0 1 0)
  paintFace textures sequence y 3 (Vector3 (-3) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures sequence z 3 (Vector3 (-3) (-3) (3)) 0 (Vector3 0 0 0)

paintEdges:: [TextureObject] -> Seq Edge -> IO()
paintEdges textures sequence = do
  paintFace textures sequence xy 10 (Vector3 (-1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures sequence fe 10 (Vector3 (-1) (-3) (-3)) 0 (Vector3 0 0 0)

  paintFace textures sequence fe 11 (Vector3 (1) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures sequence xy 11 (Vector3 (3) (-1) (-1)) 90 (Vector3 0 1 0)

  paintFace textures sequence xy 9 (Vector3 (-3) (-1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures sequence fe 9 (Vector3 (-3) (-1) (-3)) 0 (Vector3 0 0 0)

  paintFace textures sequence fe 8 (Vector3 (-1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures sequence xy 8 (Vector3 (-1) 3 (-3)) 90 (Vector3 1 0 0)

  paintFace textures sequence xy 7 (Vector3 (-3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures sequence fe 7 (Vector3 (-3) (-3) (-1)) 90 (Vector3 1 0 0)

  paintFace textures sequence fe 6 (Vector3 (1) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures sequence xy 6 (Vector3 (3) (-3) (1)) 90 (Vector3 0 1 0)

  paintFace textures sequence xy 4 (Vector3 (-3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures sequence fe 4 (Vector3 (-3) (3) (-1)) 90 (Vector3 1 0 0)

  paintFace textures sequence xy 5 (Vector3 (3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures sequence fe 5 (Vector3 (1) (3) (-1)) 90 (Vector3 1 0 0)

  paintFace textures sequence xy 2 (Vector3 (-1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures sequence fe 2 (Vector3 (-1) (-3) (3)) 0 (Vector3 0 0 0)

  paintFace textures sequence xy 1 (Vector3 (3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures sequence fe 1 (Vector3 (1) (-1) 3) 0 (Vector3 0 0 0)

  paintFace textures sequence xy 3 (Vector3 (-3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures sequence fe 3 (Vector3 (-3) (-1) 3) 0 (Vector3 0 0 0)

  paintFace textures sequence xy 0 (Vector3 (-1) 3 (1)) 90 (Vector3 1 0 0)
  paintFace textures sequence fe 0 (Vector3 (-1) (1) 3) 0 (Vector3 0 0 0)

paintCenters:: [TextureObject] -> Seq Center -> IO()
paintCenters textures sequence = do
  paintFace textures sequence center 0 (Vector3 (-1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures sequence center 1 (Vector3 (-1) 3 (-1)) 90 (Vector3 1 0 0)
  paintFace textures sequence center 2 (Vector3 3 (-1) (1)) 90 (Vector3 0 1 0)
  paintFace textures sequence center 3 (Vector3 (-3) (-1) (-1)) (-90) (Vector3 0 1 0)
  paintFace textures sequence center 4 (Vector3 (-1) (-3) (1)) (-90) (Vector3 1 0 0)
  paintFace textures sequence center 5 (Vector3 (-1) (-1) (-3)) 0 (Vector3 0 0 0)


{-|
PINTA TOOOOOOODO EL CUBO PERO PINTA DE ULTIMO LOS QUE VAN AL FRENTE
-}
tops:: [TextureObject] -> Seq Center -> Seq Edge -> Seq Corner -> IO()
tops textures centers edges corners = do
  paintFace textures centers center 3 (Vector3 (-3) (-1) (-1)) (-90) (Vector3 0 1 0)
  paintFace textures centers center 4 (Vector3 (-1) (-3) (1)) (-90) (Vector3 1 0 0)
  paintFace textures centers center 5 (Vector3 (-1) (-1) (-3)) 0 (Vector3 0 0 0)

  paintFace textures edges xy 10 (Vector3 (-1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 10 (Vector3 (-1) (-3) (-3)) 0 (Vector3 0 0 0)

  paintFace textures edges xy 9 (Vector3 (-3) (-1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 9 (Vector3 (-3) (-1) (-3)) 0 (Vector3 0 0 0)

  paintFace textures edges xy 7 (Vector3 (-3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 7 (Vector3 (-3) (-3) (-1)) 90 (Vector3 1 0 0)

  paintFace textures corners y 6 (Vector3 (-3) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners z 6 (Vector3 (-3) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 6 (Vector3 (-3) (-3) (-1)) 90 (Vector3 0 1 0)

  paintFace textures corners z 7 (Vector3 (1) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 0 (Vector3 (-3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners x 3 (Vector3 (-3) (-3) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 3 (Vector3 (-3) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners y 2 (Vector3 (1) (-3) (1)) 90 (Vector3 1 0 0)

  paintFace textures edges fe 8 (Vector3 (-1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges fe 11 (Vector3 (1) (-1) (-3)) 0 (Vector3 0 0 0)




  paintFace textures corners x 5 (Vector3 (-3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners z 5 (Vector3 (-3) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners y 5 (Vector3 (-3) (3) (-3)) 90 (Vector3 1 0 0)

  paintFace textures corners z 4 (Vector3 (1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 4 (Vector3 (3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners y 4 (Vector3 (1) (3) (-3)) 90 (Vector3 1 0 0)

  paintFace textures corners y 7 (Vector3 (1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners x 7 (Vector3 (3) (-3) (-1)) 90 (Vector3 0 1 0) -- LE MOVI Z

  paintFace textures edges xy 11 (Vector3 (3) (-1) (-1)) 90 (Vector3 0 1 0) -- LE MOVI FE

  paintFace textures edges xy 8 (Vector3 (-1) 3 (-3)) 90 (Vector3 1 0 0) -- LE MOVI FE

  paintFace textures edges fe 6 (Vector3 (1) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 6 (Vector3 (3) (-3) (1)) 90 (Vector3 0 1 0)

  paintFace textures edges xy 4 (Vector3 (-3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 4 (Vector3 (-3) (3) (-1)) 90 (Vector3 1 0 0)

  paintFace textures edges xy 5 (Vector3 (3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 5 (Vector3 (1) (3) (-1)) 90 (Vector3 1 0 0)

  paintFace textures edges xy 2 (Vector3 (-1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 2 (Vector3 (-1) (-3) (3)) 0 (Vector3 0 0 0)

  paintFace textures edges xy 1 (Vector3 (3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 1 (Vector3 (1) (-1) 3) 0 (Vector3 0 0 0)

  paintFace textures edges xy 3 (Vector3 (-3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 3 (Vector3 (-3) (-1) 3) 0 (Vector3 0 0 0)

  paintFace textures edges xy 0 (Vector3 (-1) 3 (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 0 (Vector3 (-1) (1) 3) 0 (Vector3 0 0 0)

  paintFace textures centers center 0 (Vector3 (-1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures centers center 1 (Vector3 (-1) 3 (-1)) 90 (Vector3 1 0 0)
  paintFace textures centers center 2 (Vector3 3 (-1) (1)) 90 (Vector3 0 1 0)

  paintFace textures corners y 0 (Vector3 (-3) (3) (1)) 90 (Vector3 1 0 0) -- LE MOVI X
  paintFace textures corners z 0 (Vector3 (-3) (1) (3)) 0 (Vector3 0 0 0)

  paintFace textures corners x 1 (Vector3 (3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 1 (Vector3 (1) (3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners z 1 (Vector3 (1) (1) (3)) 0 (Vector3 0 0 0)

  paintFace textures corners x 2 (Vector3 (3) (-3) (3)) 90 (Vector3 0 1 0) -- LE MOVI Y
  paintFace textures corners z 2 (Vector3 (1) (-3) (3)) 0 (Vector3 0 0 0)

  paintFace textures corners z 3 (Vector3 (-3) (-3) (3)) 0 (Vector3 0 0 0) -- LE MOVI X y Y

paintFace:: (Integral b) => [TextureObject] -> Seq a -> (a -> b) -> Int -> (Vector3 GLfloat) -> GLfloat -> (Vector3 GLfloat) -> IO ()
paintFace textures sequence f indice trans deg rot =
      preservingMatrix $ do
        let val = index sequence indice
            t   = faceTexture textures $ (f val) `div` 10
        translate trans
        rotate deg rot
        face t

paintColum2:: [TextureObject] -> RCube -> IO()
paintColum2 textures (RCube corners edges centers)  = do
  paintFace textures centers center 3 (Vector3 (-3) (-1) (-1)) (-90) (Vector3 0 1 0)
  paintFace textures edges xy 9 (Vector3 (-3) (-1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 9 (Vector3 (-3) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 7 (Vector3 (-3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 7 (Vector3 (-3) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures corners y 6 (Vector3 (-3) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners z 6 (Vector3 (-3) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 6 (Vector3 (-3) (-3) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners x 0 (Vector3 (-3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners x 3 (Vector3 (-3) (-3) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 3 (Vector3 (-3) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners x 5 (Vector3 (-3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners z 5 (Vector3 (-3) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners y 5 (Vector3 (-3) (3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 4 (Vector3 (-3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 4 (Vector3 (-3) (3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 3 (Vector3 (-3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 3 (Vector3 (-3) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures corners y 0 (Vector3 (-3) (3) (1)) 90 (Vector3 1 0 0) -- LE MOVI X
  paintFace textures corners z 0 (Vector3 (-3) (1) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners z 3 (Vector3 (-3) (-3) (3)) 0 (Vector3 0 0 0) -- LE MOVI X y Y

paintColum1:: [TextureObject] -> RCube -> IO()
paintColum1 textures (RCube corners edges centers)  = do
  paintFace textures centers center 4 (Vector3 (-1) (-3) (1)) (-90) (Vector3 1 0 0)
  paintFace textures centers center 5 (Vector3 (-1) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 10 (Vector3 (-1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 10 (Vector3 (-1) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges fe 8 (Vector3 (-1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 8 (Vector3 (-1) 3 (-3)) 90 (Vector3 1 0 0) -- LE MOVI FE
  paintFace textures edges xy 2 (Vector3 (-1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 2 (Vector3 (-1) (-3) (3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 0 (Vector3 (-1) 3 (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 0 (Vector3 (-1) (1) 3) 0 (Vector3 0 0 0)
  paintFace textures centers center 0 (Vector3 (-1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures centers center 1 (Vector3 (-1) 3 (-1)) 90 (Vector3 1 0 0)

paintColum0:: [TextureObject] -> RCube -> IO()
paintColum0 textures (RCube corners edges centers)  = do
  paintFace textures edges xy 11 (Vector3 (3) (-1) (-1)) 90 (Vector3 0 1 0) -- LE MOVI FE
  paintFace textures edges fe 6 (Vector3 (1) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 6 (Vector3 (3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures edges xy 5 (Vector3 (3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 5 (Vector3 (1) (3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 1 (Vector3 (3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 1 (Vector3 (1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures corners z 4 (Vector3 (1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 4 (Vector3 (3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners y 4 (Vector3 (1) (3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners y 7 (Vector3 (1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners x 7 (Vector3 (3) (-3) (-1)) 90 (Vector3 0 1 0) -- LE MOVI Z
  paintFace textures corners x 1 (Vector3 (3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 1 (Vector3 (1) (3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners z 1 (Vector3 (1) (1) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 2 (Vector3 (3) (-3) (3)) 90 (Vector3 0 1 0) -- LE MOVI Y
  paintFace textures corners z 2 (Vector3 (1) (-3) (3)) 0 (Vector3 0 0 0)
  paintFace textures centers center 2 (Vector3 3 (-1) (1)) 90 (Vector3 0 1 0)

paintRow0:: [TextureObject] -> RCube -> IO()
paintRow0 textures (RCube corners edges centers)  = do
  paintFace textures corners x 0 (Vector3 (-3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners x 3 (Vector3 (-3) (-3) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 3 (Vector3 (-3) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners y 2 (Vector3 (1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 2 (Vector3 (-1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 2 (Vector3 (-1) (-3) (3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 1 (Vector3 (3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 1 (Vector3 (1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures edges xy 3 (Vector3 (-3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 3 (Vector3 (-3) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures edges xy 0 (Vector3 (-1) 3 (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 0 (Vector3 (-1) (1) 3) 0 (Vector3 0 0 0)
  paintFace textures centers center 0 (Vector3 (-1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures corners y 0 (Vector3 (-3) (3) (1)) 90 (Vector3 1 0 0) -- LE MOVI X
  paintFace textures corners z 0 (Vector3 (-3) (1) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 1 (Vector3 (3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 1 (Vector3 (1) (3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners z 1 (Vector3 (1) (1) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 2 (Vector3 (3) (-3) (3)) 90 (Vector3 0 1 0) -- LE MOVI Y
  paintFace textures corners z 2 (Vector3 (1) (-3) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners z 3 (Vector3 (-3) (-3) (3)) 0 (Vector3 0 0 0) -- LE MOVI X y Y

paintRow1:: [TextureObject] -> RCube -> IO()
paintRow1 textures (RCube _ edges centers)  = do
  paintFace textures centers center 3 (Vector3 (-3) (-1) (-1)) (-90) (Vector3 0 1 0)
  paintFace textures centers center 4 (Vector3 (-1) (-3) (1)) (-90) (Vector3 1 0 0)
  paintFace textures edges xy 7 (Vector3 (-3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 7 (Vector3 (-3) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 6 (Vector3 (1) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 6 (Vector3 (3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures edges xy 4 (Vector3 (-3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 4 (Vector3 (-3) (3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 5 (Vector3 (3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 5 (Vector3 (1) (3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures centers center 1 (Vector3 (-1) 3 (-1)) 90 (Vector3 1 0 0)
  paintFace textures centers center 2 (Vector3 3 (-1) (1)) 90 (Vector3 0 1 0)

paintRow2:: [TextureObject] -> RCube -> IO()
paintRow2 textures (RCube corners edges centers)  = do
  paintFace textures centers center 5 (Vector3 (-1) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 10 (Vector3 (-1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 10 (Vector3 (-1) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 9 (Vector3 (-3) (-1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 9 (Vector3 (-3) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners y 6 (Vector3 (-3) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners z 6 (Vector3 (-3) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 6 (Vector3 (-3) (-3) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners z 7 (Vector3 (1) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges fe 8 (Vector3 (-1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges fe 11 (Vector3 (1) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 5 (Vector3 (-3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners z 5 (Vector3 (-3) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners y 5 (Vector3 (-3) (3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners z 4 (Vector3 (1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 4 (Vector3 (3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners y 4 (Vector3 (1) (3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners y 7 (Vector3 (1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners x 7 (Vector3 (3) (-3) (-1)) 90 (Vector3 0 1 0) -- LE MOVI Z
  paintFace textures edges xy 11 (Vector3 (3) (-1) (-1)) 90 (Vector3 0 1 0) -- LE MOVI FE
  paintFace textures edges xy 8 (Vector3 (-1) 3 (-3)) 90 (Vector3 1 0 0) -- LE MOVI FE

paintLayer0:: [TextureObject] -> RCube -> IO()
paintLayer0 textures (RCube corners edges centers)  = do
  paintFace textures edges fe 8 (Vector3 (-1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 5 (Vector3 (-3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners z 5 (Vector3 (-3) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners y 5 (Vector3 (-3) (3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners z 4 (Vector3 (1) (1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 4 (Vector3 (3) (1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners y 4 (Vector3 (1) (3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 8 (Vector3 (-1) 3 (-3)) 90 (Vector3 1 0 0) -- LE MOVI FE
  paintFace textures edges xy 4 (Vector3 (-3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 4 (Vector3 (-3) (3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 5 (Vector3 (3) 1 (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 5 (Vector3 (1) (3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 0 (Vector3 (-1) 3 (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 0 (Vector3 (-1) (1) 3) 0 (Vector3 0 0 0)
  paintFace textures centers center 1 (Vector3 (-1) 3 (-1)) 90 (Vector3 1 0 0)
  paintFace textures corners y 0 (Vector3 (-3) (3) (1)) 90 (Vector3 1 0 0) -- LE MOVI X
  paintFace textures corners z 0 (Vector3 (-3) (1) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 1 (Vector3 (3) (1) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 1 (Vector3 (1) (3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners z 1 (Vector3 (1) (1) (3)) 0 (Vector3 0 0 0)

paintLayer1:: [TextureObject] -> RCube -> IO()
paintLayer1 textures (RCube corners edges centers)  = do
  paintFace textures centers center 3 (Vector3 (-3) (-1) (-1)) (-90) (Vector3 0 1 0)
  paintFace textures centers center 5 (Vector3 (-1) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 9 (Vector3 (-3) (-1) (-1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 9 (Vector3 (-3) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges fe 11 (Vector3 (1) (-1) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 11 (Vector3 (3) (-1) (-1)) 90 (Vector3 0 1 0) -- LE MOVI FE
  paintFace textures edges xy 1 (Vector3 (3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 1 (Vector3 (1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures edges xy 3 (Vector3 (-3) (-1) (3)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 3 (Vector3 (-3) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures centers center 0 (Vector3 (-1) (-1) 3) 0 (Vector3 0 0 0)
  paintFace textures centers center 2 (Vector3 3 (-1) (1)) 90 (Vector3 0 1 0)

paintLayer2:: [TextureObject] -> RCube -> IO()
paintLayer2 textures (RCube corners edges centers)  = do
  paintFace textures centers center 4 (Vector3 (-1) (-3) (1)) (-90) (Vector3 1 0 0)
  paintFace textures edges xy 10 (Vector3 (-1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 10 (Vector3 (-1) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures edges xy 7 (Vector3 (-3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures edges fe 7 (Vector3 (-3) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures corners y 6 (Vector3 (-3) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners z 6 (Vector3 (-3) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 6 (Vector3 (-3) (-3) (-1)) 90 (Vector3 0 1 0)
  paintFace textures corners z 7 (Vector3 (1) (-3) (-3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 3 (Vector3 (-3) (-3) (3)) 90 (Vector3 0 1 0)
  paintFace textures corners y 3 (Vector3 (-3) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners y 2 (Vector3 (1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures corners y 7 (Vector3 (1) (-3) (-3)) 90 (Vector3 1 0 0)
  paintFace textures corners x 7 (Vector3 (3) (-3) (-1)) 90 (Vector3 0 1 0) -- LE MOVI Z
  paintFace textures edges fe 6 (Vector3 (1) (-3) (-1)) 90 (Vector3 1 0 0)
  paintFace textures edges xy 6 (Vector3 (3) (-3) (1)) 90 (Vector3 0 1 0)
  paintFace textures edges xy 2 (Vector3 (-1) (-3) (1)) 90 (Vector3 1 0 0)
  paintFace textures edges fe 2 (Vector3 (-1) (-3) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners x 2 (Vector3 (3) (-3) (3)) 90 (Vector3 0 1 0) -- LE MOVI Y
  paintFace textures corners z 2 (Vector3 (1) (-3) (3)) 0 (Vector3 0 0 0)
  paintFace textures corners z 3 (Vector3 (-3) (-3) (3)) 0 (Vector3 0 0 0) -- LE MOVI X y Y

--paintCubeByLayers



