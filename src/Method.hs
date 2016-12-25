module Method where

type XYFunction = Double -> Double -> Double
type SingleFunction = (Double -> Double)
type FunctionParams = (Double,Double)

newton :: FunctionParams -> Double -> XYFunction -> XYFunction -> XYFunction -> XYFunction -> XYFunction -> XYFunction -> FunctionParams
newton (x,y) e f g dxf dxg dyf dyg
    | ((abs (f x y))<e) && ((abs (g x y))< e) = (x,y)
    | otherwise = newton (x + dx,y + dy) e f g dxf dxg dyf dyg where
        dx = ((dyf x y)*(g x y) - (f x y)*(dyg x y))/((dxf x y)*(dyg x y) - (dyf x y)*(dxg x y))
        dy = ((f x y)*(dxg x y) - (dxf x y)*(g x y))/((dxf x y)*(dyg x y) - (dyf x y)*(dxg x y))

iterationMethod  :: FunctionParams -> Double -> XYFunction -> XYFunction -> XYFunction -> FunctionParams
iterationMethod (x,y) e f dxf dyf
    | (abs (f x y))<e  = (x,y)
    | otherwise = iterationMethod (x+dx,y+dy) e f dxf dyf where
        dx = x - (dxf x y)
        dy = y - (dyf x y)


iterationMetod :: FunctionParams -> Double -> XYFunction -> XYFunction -> XYFunction -> XYFunction -> XYFunction -> XYFunction -> FunctionParams
iterationMetod (x,y) e f g dxf dxg dyf dyg
    | ((abs (f x y))<e) && ((abs (g x y))< e) = (x,y)
    | otherwise = newton (x + dx,y + dy) e f g dxf dxg dyf dyg where
        dx = ((dyf x y)*(g x y) - (f x y)*(dyg x y))/((dxf x y)*(dyg x y) - (dyf x y)*(dxg x y))
        dy = ((f x y)*(dxg x y) - (dxf x y)*(g x y))/((dxf x y)*(dyg x y) - (dyf x y)*(dxg x y))