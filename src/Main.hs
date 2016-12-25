module Main where

import Prelude

import Util

import Graphics.UI.GLUT

import MyMathUtil

import Method


enablePinkColor  =  color3f 0.349019607843137 0.094117647058824 0.47843137254902
enableTealColor  =  color3f 0 0.3764705882 0.3450980392

function1 :: Double -> Double
function1 y = (y*y)/(1-y)

function2 :: Double -> Double
function2 x = x*x/(4-x)

functionF x y = x*x+x*y-4*y
functionFdx x y = 2*x+y
functionFdy x y = x-4

functionG x y = x - x*y - y*y
functionGdx _ y = 1-y
functionGdy x y = -x - 2*y

commonFunction x y = x*x+x*y-4*y - (x - x*y - y*y)
commonFunctiondx x y = 2*x+y - (1 - y )
commonFunctiondy x y = y-4 - (- x - 2*y)

e = 0.001

main :: IO ()
main = do
  putStrLn (show (duplicateListFrom 1 [0,1 .. 5]))
  putStrLn (show (newton (-5,3) e functionF functionG functionFdx functionGdx functionFdy functionGdy))
  putStrLn (show (newton (2,1) e functionF functionG functionFdx functionGdx functionFdy functionGdy))
  putStrLn (show (iterationMetod (-5,3) e functionF functionG functionFdx functionGdx functionFdy functionGdy))
  putStrLn (show (iterationMetod (2,1) e functionF functionG functionFdx functionGdx functionFdy functionGdy))
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "lab #2"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  pointSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  pointSize $= 7
  drawSurface
  enablePinkColor
  lineWidth $=2
  drawGrafic (duplicateListFrom 1 graphic1)
  enableTealColor
  lineWidth $=1
  drawGrafic (duplicateListFrom 1 graphic2)
  flush


drawSurface = do
  color3f 0.4 0.4 0.4
  renderPrimitive Lines $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) template
  color3f 1 1 1

step = 0.05
size = 0.01

template :: [(Double,Double,Double)]
template = [(-1,0,0),(1,0,0),(0,-1,0),(0,1,0)] ++
   concat [[(k,-size,0),(k,size,0),(-size,k,0),(size,k,0)] | k<-[-1,-1 + step .. 1]]

graphic1 :: [(Double,Double,Double)]
graphic1 = filterList 0.1 [((function1 k)*step,k*step,0) | k<-[-20, -19.99 .. 20]]

graphic2 :: [(Double,Double,Double)]
graphic2 = filterList 0.1 [(k*step,(function2 k)*step,0) | k<-[-20, -19.99 .. 20]]

label :: Double -> Double -> [(Double,Double,Double)]
label position step = [(position,-step,0),(position,step,0),(step,position,0),(-step,position,0)]

drawGrafic :: [(Double,Double,Double)] -> IO ()
drawGrafic graphic = do
  renderPrimitive Lines $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) graphic

