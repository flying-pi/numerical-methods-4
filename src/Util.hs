module Util where

import Graphics.UI.GLUT

forEach :: (Num i) => (a->b->i->a)->a->i->[b]->a
forEach _ init _ [] = init
forEach fun init iterator (head:tail) = forEach fun (fun init head iterator) (iterator + 1) tail

fixpoint f x = f (fixpoint f) x

firstElemenetFromListOfList :: [[a]] -> [a]
firstElemenetFromListOfList [] = []
firstElemenetFromListOfList ((first:_):tail) = [first] ++ (firstElemenetFromListOfList tail)

generateDy::[Double] -> [[Double]]
generateDy (head:[]) = []
generateDy source = [newCollumn] ++ (generateDy newCollumn)
 where newCollumn = delta source

delta :: [Double] -> [Double]
delta previesIterration = let (y1:tail) = previesIterration
                              (_:(y2:_)) = previesIterration
                          in if((length previesIterration)<2) then [] else [y2-y1] ++ (delta tail)

data IterationState = IterationState {
    n :: Double,
    buf :: Double,
    stepValue :: Double
}

color3f r g b = color $ Color3 r g (b :: GLfloat)

concatListToSingleValue :: (RealFrac a) =>  (a -> a -> a) -> [a] -> a -> a
concatListToSingleValue _ [] state = state
concatListToSingleValue operator (head:tail) state = concatListToSingleValue  operator tail (operator head state )

mergeList :: (RealFrac a) =>  (a -> a -> a) -> [a] -> [a] -> [a]
mergeList _ [] _ = []
mergeList operator (head1:tail1) (head2:tail2) = (operator head1 head2):(mergeList operator tail1 tail2 )

duplicateListFrom :: Integer -> [a] -> [a]
duplicateListFrom _ [] = []
duplicateListFrom 0 list = duplicateList list
duplicateListFrom num (head:tail) = head:(duplicateListFrom (num-1) tail)

duplicateList :: [a] -> [a]
duplicateList [] = []
duplicateList (head:tail) = head:head:(duplicateList tail)

filterList :: Double -> [(Double,Double,Double)] -> [(Double,Double,Double)]
filterList _ [] = []
filterList _ (first:[]) = []
filterList e ((a1,b1,c1):((a2,b2,c2):tail)) = if(((abs (a1 - a2)) > e) || ((abs (b1 - b2)) > e) || ((abs (c1 - c2)) > e)) then
        filterList e tail
    else
        (a1,b1,c1):((a2,b2,c2):(filterList e tail))