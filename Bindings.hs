module Bindings (keyboardMouse, CubeState(..), goalState, Inst(..), randomMoves ) where

import Graphics.UI.GLUT hiding (R,T)
import Display
import Data.IORef
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Control.Monad


keyboardMouse inst rot insts key state modifiers position = do
  keyboardAct inst rot insts key state modifiers

keyboardAct inst _ _ (Char 'f') Down a = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [F])
keyboardAct inst _ _ (Char 'F') Down a = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [F'])
keyboardAct inst _ _ (Char 'b') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [B])
keyboardAct inst _ _ (Char 'B') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [B'])
keyboardAct inst _ _ (Char 'r') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [R])
keyboardAct inst _ _ (Char 'R') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [R'])
keyboardAct inst _ _ (Char 'l') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [L])
keyboardAct inst _ _ (Char 'L') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [L'])
keyboardAct inst _ _ (Char 'u') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [U])
keyboardAct inst _ _ (Char 'U') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [U'])
keyboardAct inst _ _ (Char 'd') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [D])
keyboardAct inst _ _ (Char 'D') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [D'])
keyboardAct inst rot _ (Char 'E') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [TF])
keyboardAct inst rot _ (Char 'e') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [TF']) 
keyboardAct inst rot _ (Char 'W') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [TU])
keyboardAct inst rot _ (Char 'w') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [TU'])
keyboardAct inst rot _ (Char 'Q') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [TR])
keyboardAct inst rot _ (Char 'q') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ writeIORef inst ((tail i) ++ [TR'])
keyboardAct inst rot _ (Char 'P') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ do
    movements <- randomMoves 3
    let moves = concat $ movements
    print $ length moves
    writeIORef inst ((tail i) ++ moves)
keyboardAct inst rot insts (Char 'S') Down _ = do
  i <- readIORef inst
  when (head i == NO) $ do
    l <- readIORef insts
    writeIORef insts [NO]
    writeIORef inst l
keyboardAct _ _ _ (Char '\27') Down _ = exitWith ExitSuccess
keyboardAct _ _ _ _ _ _ = return ()


