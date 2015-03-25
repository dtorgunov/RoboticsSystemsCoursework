import Data.List
import Data.Number.CReal

-- We define a matrix to be a nested list
type Vector a = [a]
type Matrix a = [Vector a]
type Domain = Double


-- The axis parameters and toolset coordinates we're working with
d1 = 160.0
a2 = 100.0
a3 = 100.0
d5 = 50.0

parameters :: [ [Domain] ]
parameters = [ [d1, 0.0, -90],
               [0.0, a2, 0.0],
               [0.0, a3, 0.0],
               [0.0, 0.0, -90],
               [d5, 0.0, 0.0] ]

tool :: Matrix Domain
tool = [[10.0], [0.0], [10.0], [1.0]]

-- Dot product
(<.>) :: Num a => Vector a -> Vector a -> a
n <.> m = sum $ map (uncurry (*)) $ zip n m
    
-- Matrix multiplication
(<*>) :: Num a => Matrix a -> Matrix a -> Matrix a
m <*> n = [ [ sum $ zipWith (*) mr nc | nc <- (transpose n) ] | mr <- m ]
         
-- Theta-variable DH-matrix
dh [d, a, alphaDeg] = \thetaDeg ->
                      let theta = degToRad thetaDeg
                          alpha = degToRad alphaDeg
                      in
                        [ [ (cos theta), (-1) * (cos alpha) * (sin theta),  (sin alpha) * (sin theta), a * (cos theta) ],
                      [ (sin theta), (cos alpha) * (cos theta), (-1) * (sin alpha) * (cos theta), a * (sin theta) ],
                      [ 0, (sin alpha), (cos alpha), d ],
                      [ 0, 0, 0, 1] ]


-- Applies a list of functions to a list of arguments
applyList :: [(a -> b)]  -> [a] -> [b]
f `applyList` arg = map (\(f, x) -> f x) $ zip f arg

-- Gets the final transform
accumulate :: (Num a) => [Matrix a] -> Matrix a
accumulate = foldr (\x acc -> x <*> acc) [ [1,0,0,0]
                                         , [0,1,0,0]
                                         , [0,0,1,0]
                                         , [0,0,0,1]]

-- example :: [CReal]
-- example = [0, 0, 0, -90, 0]

degToRad :: (Floating a) => a -> a
degToRad deg = deg * pi / 180

test example = (accumulate $ (map dh parameters) `applyList` example) <*> tool
