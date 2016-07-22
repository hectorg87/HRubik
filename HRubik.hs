module Main where

import Graphics.UI.GLUT
import Bindings
import Display
import Data.IORef
import Control.Monad
import Data.Maybe
import Sprites
import Control.Monad(when)
import System.Exit(exitFailure)
import System.IO(withBinaryFile, IOMode(ReadMode), openBinaryFile, hGetBuf)
import Foreign.Marshal.Alloc(allocaBytes, mallocBytes)

loadTexture :: FilePath -> IO TextureObject
loadTexture f = do
  withBinaryFile f ReadMode $ \h -> do
    let bytes = 256 * 256 * 4
    allocaBytes bytes $ \pixels -> do
      bytes' <- hGetBuf h pixels bytes
      when (bytes' /= bytes) exitFailure
      [tex] <- genObjectNames 1
      texture Texture2D $= Enabled
      textureBinding Texture2D $= Just tex
      build2DMipmaps Texture2D RGBA' 256 256
        (PixelData RGBA UnsignedByte pixels)
      textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
      textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
      textureBinding Texture2D $= Nothing
      texture Texture2D $= Disabled
      return tex

myInit :: IO [TextureObject]
myInit = do
--   lighting $= Enabled
--   position (Light 0) $= Vertex4 0.8 0 3.0 5.0
--   light (Light 0) $= Enabled
   clearColor $= Color4 0.5 0.5 0.5 0
   shadeModel $= Smooth
   t0 <- loadTexture "img/Blue256.png.rgba"
   t1 <- loadTexture "img/Red256.png.rgba"
   t2 <- loadTexture "img/Green256.png.rgba"
   t3 <- loadTexture "img/White256.png.rgba"
   t4 <- loadTexture "img/Orange256.png.rgba"
   t5 <- loadTexture "img/Yellow256.png.rgba"
   texture Texture2D $= Enabled
   return [t0,t1,t2,t3,t4,t5]


main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 100
  createWindow progName
  textures <- myInit
  reshapeCallback $= Just reshape

  initial  <- newIORef goalState
  empty    <- newIORef [NO]
  noInst   <- newIORef [NO]
  noRot    <- newIORef 0.0

  let state = CS { cube = initial, insts = empty, inst = noInst, rot = noRot}


  keyboardMouseCallback $= Just (keyboardMouse noInst noRot empty)
  idleCallback $= Just idle
  displayCallback $= (display state textures)
  mainLoop

