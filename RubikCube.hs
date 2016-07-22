module RubikCube ( RCube(..), Corner(..), Edge(..), Center(..), Inst(..), CubeState(..), CFace(..), nextMove, goalState, randomMoves, inverseMove ) where

import Data.IORef
import Data.Sequence
import Data.Word (Word8)
import Test.QuickCheck
import Graphics.UI.GLUT hiding (index,R)

data CubeState = CS { cube  :: IORef RCube,
                      insts :: IORef [Inst],
                      inst :: IORef [Inst],
                      rot  :: IORef GLfloat
                      }

data RCube = RCube { _corners :: !Corners,
                   _edges :: !Edges,
                   _centers :: !Centers
                   }
  deriving(Eq, Show)

type Corners = Seq Corner

type Edges = Seq Edge

type Centers = Seq Center

data Corner = Corner { z :: !CFace, -- ^ Z axis
                       y :: !CFace, -- ^ Y axis
                       x :: !CFace  -- ^ X axis
                       }
  deriving(Eq)

instance Show Corner where
  show (Corner z y x) = show "Z: " ++ show z ++ show ", Y: "
            ++ show y ++ show ", X: " ++ show x ++ "\n"

data Edge = Edge { fe :: !CFace, -- ^ Front Edge
                   xy :: !CFace  -- ^ X or Y axis
                   }
  deriving(Eq)

instance Show Edge where
  show (Edge fe xy) = show "fe: " ++ show fe ++ show ", XY: "
            ++ show xy ++ "\n"

data Center = Center { center :: !CFace -- ^ Center tile
                       }
  deriving(Eq)

instance Show Center where
  show (Center center) = show "center: " ++ show center ++ "\n"

type CFace = Word8

type Color = String

{-| @Inst@ Conjunto de itrucciones para manejar
    el cubo de Rubik.
-}
data Inst = NO  -- ^ NO Move
          | SH  -- ^ Shuffle Cube
          | F   -- ^ Rotate Front Clockwise                  f in the Keyboard
          | F'  -- ^ Rotate Front CounterClockwise           F in the Keyboard
          | R   -- ^ Rotate Right Clockwise                  r in the Keyboard
          | R'  -- ^ Rotate Right CounterClockwise           R in the Keyboard
          | L   -- ^ Rotate Left Clockwise                   l in the Keyboard
          | L'  -- ^ Rotate Left CounterClockwise            L in the Keyboard
          | B   -- ^ Rotate Back Clockwise                   b in the Keyboard
          | B'  -- ^ Rotate Back CounterClockwise            B in the Keyboard
          | U   -- ^ Rotate Up Clockwise                     u in the Keyboard
          | U'  -- ^ Rotate Up CounterClockwise              U in the Keyboard
          | D   -- ^ Rotate Down Clockwise                   d in the Keyboard
          | D'  -- ^ Rotate Down CounterClockwise            D in the Keyboard
          | MF  -- ^ Rotate Middle Front Clockwise
          | MF' -- ^ Rotate Middle Front CounterClockwise
          | MU  -- ^ Rotate Middle Up Clockwise
          | MU' -- ^ Rotate Middle Up CounterClockwise
          | MR  -- ^ Rotate Middle Right Clockwise
          | MR' -- ^ Rotate Middle Right CounterClockwise
          | TF  -- ^ Turn Cube ClockWise on Z axis
          | TF' -- ^ Turn Cube CounterClockwise on Z axis
          | TU  -- ^ Turn Cube ClockWise on Y axis
          | TU' -- ^ Turn Cube CounterClockwise on Y axis
          | TR  -- ^ Turn Cube ClockWise on X axis
          | TR' -- ^ Turn Cube CounterClockwise on X axis
  deriving(Eq,Show)

inverseMove:: Inst -> Inst
inverseMove F   = F'
inverseMove F'  = F
inverseMove R   = R'
inverseMove R'  = R
inverseMove L   = L'
inverseMove L'  = L
inverseMove B   = B'
inverseMove B'  = B
inverseMove U   = U'
inverseMove U'  = U
inverseMove D   = D'
inverseMove D'  = D
inverseMove MF  = MF'
inverseMove MF' = MF
inverseMove MU  = MU'
inverseMove MU' = MU
inverseMove MR  = MR'
inverseMove MR' = MR
inverseMove TF  = TF'
inverseMove TF' = TF
inverseMove TU  = TU'
inverseMove TU' = TU
inverseMove TR  = TR'
inverseMove TR' = TR

instance Arbitrary Inst where
  arbitrary = elements [F, F', R, R', L, L', B, B', U, U', D, D',
                        MF, MF', MU, MU', MR, MR',
                        TF, TF', TU, TU', TR, TR']

goalState = let ful = Corner { z =  1, y = 17, x = 23 }
                fur = Corner { z =  3, y = 19, x = 41 }
                fdr = Corner { z =  9, y = 33, x = 47 }
                fdl = Corner { z =  7, y = 31, x = 29 }
                bul = Corner { z = 51, y = 13, x = 43 }
                bur = Corner { z = 53, y = 11, x = 21 }
                bdr = Corner { z = 59, y = 37, x = 27 }
                bdl = Corner { z = 57, y = 39, x = 49 }
                fu  = Edge { fe =  2, xy = 18 }
                fr  = Edge { fe =  6, xy = 44 }
                fd  = Edge { fe =  8, xy = 32 }
                fl  = Edge { fe =  4, xy = 26 }
                mu  = Edge { fe = 14, xy = 22 }
                mr  = Edge { fe = 16, xy = 42 }
                md  = Edge { fe = 36, xy = 48 }
                ml  = Edge { fe = 34, xy = 28 }
                bu  = Edge { fe = 52, xy = 12 }
                br  = Edge { fe = 56, xy = 24 }
                bd  = Edge { fe = 58, xy = 38 }
                bl  = Edge { fe = 54, xy = 46 }
                f   = Center 5
                u   = Center 15
                r   = Center 45
                l   = Center 25
                d   = Center 35
                b   = Center 55
            in RCube (fromList [ful, fur, fdr, fdl,
                               bul, bur, bdr, bdl])
                    (fromList [fu, fr, fd, fl,
                               mu, mr, md, ml, bu, br, bd, bl])
                    (fromList [f,u,r,l,d,b])

nextMove:: Inst -> CubeState -> IO ()
nextMove inst state = do
  let prevCube = cube state
  let list = insts state
  c <- readIORef prevCube
  l <- readIORef list
  let newCube = move inst c
  writeIORef prevCube newCube
  writeIORef list (inst:l)
--  print newCube
  return ()

{-| @move@ funcion que interpreta la instrucción de movimiento
    a realizarse en el cubo y la realiza.
-}
move:: Inst -> RCube -> RCube
move inst cube =
  let corners = _corners cube
      edges   = _edges cube
      centers = _centers cube
  in case inst of
      F   -> RCube (rotateCube [0,1,2,3] (swap 0) corners)
                  (rotateCube [0,1,2,3]   id edges) centers
      F'  -> RCube (rotateCube [3,2,1,0] (swap 0) corners)
                  (rotateCube [3,2,1,0]   id edges) centers
      R   -> RCube (rotateCube [1,4,7,2] (swap 1) corners)
                  (rotateCube [1,5,11,6]  id edges) centers
      R'  -> RCube (rotateCube [2,7,4,1] (swap 1) corners)
                  (rotateCube [6,11,5,1]  id edges) centers
      L   -> RCube (rotateCube [0,3,6,5] (swap 1) corners)
                  (rotateCube [3,7,9,4]   id edges) centers
      L'  -> RCube (rotateCube [5,6,3,0] (swap 1) corners)
                  (rotateCube [4,9,7,3]   id edges) centers
      B   -> RCube (rotateCube [4,5,6,7] (swap 0) corners)
                  (rotateCube [8,9,10,11] id edges) centers
      B'  -> RCube (rotateCube [7,6,5,4] (swap 0) corners)
                  (rotateCube [11,10,9,8] id edges) centers
      U   -> RCube (rotateCube [0,5,4,1] (swap 2) corners)
                  (rotateCube [0,4,8,5]   swapEdge edges) centers
      U'  -> RCube (rotateCube [1,4,5,0] (swap 2) corners)
                  (rotateCube [5,8,4,0]   swapEdge edges) centers
      D   -> RCube (rotateCube [3,2,7,6] (swap 2) corners)
                  (rotateCube [2,6,10,7]  swapEdge edges) centers
      D'  -> RCube (rotateCube [6,7,2,3] (swap 2) corners)
                  (rotateCube [7,10,6,2]  swapEdge edges) centers
      MF  -> RCube corners (rotateCube [4,5,6,7]  swapEdge edges)
                  (rotateCube [1,2,4,3] id centers)
      MF' -> RCube corners (rotateCube [7,6,5,4]  swapEdge edges)
                  (rotateCube [3,4,2,1] id centers)
      MU  -> RCube corners (rotateCube [1,3,9,11] swapEdge edges)
                  (rotateCube [0,3,5,2] id centers)
      MU' -> RCube corners (rotateCube [11,9,3,1] swapEdge edges)
                  (rotateCube [2,5,3,0] id centers)
      MR  -> RCube corners (rotateCube [0,8,10,2] swapEdge edges)
                  (rotateCube [0,1,5,4] id centers)
      MR' -> RCube corners (rotateCube [2,10,8,0] swapEdge edges)
                  (rotateCube [4,5,1,0] id centers)
      TF  -> move B' $ move MF  $ move F  cube
      TF' -> move B  $ move MF' $ move F' cube
      TU  -> move D' $ move MU  $ move U  cube
      TU' -> move D  $ move MU' $ move U' cube
      TR  -> move L' $ move MR  $ move R  cube
      TR' -> move L  $ move MR' $ move R' cube
      NO  -> cube

{-| @rotateCube@ realiza la rotacion de una secuencia de esquinas
    o lados del cubo.
-}
rotateCube:: [Word8] -> (a -> a) -> (Seq a) -> (Seq a)
rotateCube list swap sequence =
  let [i,j,k,l] = map (fromIntegral) list
      a = index sequence i
      b = index sequence j
      c = index sequence k
      d = index sequence l
  in update l (swap c) $ update k (swap b) $ update j (swap a)
          $ update i (swap d) sequence

{-| @swapEdge@ realiza el intercambio de valores en las caras
    de un lado del cubo.
-}
swapEdge:: Edge -> Edge
swapEdge edge = edge { fe = xy edge,
                       xy = fe edge
                       }
{-| @swap@ realiza el intercambio de valores en las caras de
    una esquina del cubo.
-}
swap:: Word8 -> Corner -> Corner
swap 0 corner = corner { y = x corner,
                         x = y corner
                         }
swap 1 corner = corner { y = z corner,
                         z = y corner
                         }
swap 2 corner = corner { z = x corner,
                         x = z corner
                         }
swap _ corner = error ("Por alguna razón invocamos a swap" ++
          "con un valor invalido")

randomMoves n = sample' $ vectorOf n $ (arbitrary :: Gen Inst)
