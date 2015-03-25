import Data.List
import Data.Number.CReal

-- We define a matrix to be a nested list
type Vector a = [a]
type Matrix a = [Vector a]
type Domain = Double


-- INPUTS
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

-- Matrix manipulation
-- Dot product
(<.>) :: Num a => Vector a -> Vector a -> a
n <.> m = sum $ zipWith (*) n m
    
-- Matrix multiplication
(<*>) :: Num a => Matrix a -> Matrix a -> Matrix a
m <*> n = [ [ mrows <.> ncolumns | ncolumns <- (transpose n) ] | mrows <- m ]

-- Define an n-by-n identity matrix
i :: Num a => Integer -> Matrix a
i n = map (\x -> map kronecker x) positions
    where
      -- Kronecker delta function
      kronecker (i, j) | i == j = 1
                       | otherwise = 0
      positions = [[(i,j) | j <- [1 .. n]] | i <- [1 .. n]]
         
-- Denavit-Hartenberg matrix
degToRad :: (Floating a) => a -> a
degToRad deg = deg * pi / 180

-- Theta-variable DH-matrix
dh [d, a, alphaDeg]
    = \thetaDeg ->
      let theta = degToRad thetaDeg
          alpha = degToRad alphaDeg
      in
        [ [ (c theta), (-1)*(c alpha)*(s theta),      (s alpha)*(s theta), a*(c theta) ]
        , [ (s theta),      (c alpha)*(c theta), (-1)*(s alpha)*(c theta), a*(s theta) ]
        , [         0,                (s alpha),                (c alpha),           d ]
        , [         0,                        0,                        0,           1 ] ]
    where
      c = cos
      s = sin

-- Data combinatiors
-- Applies a list of functions to a list of arguments
applyList :: [(a -> b)]  -> [a] -> [b]
fs `applyList` arg = zipWith ($) fs arg

-- Gets the final transform
accumulate :: (Num a) => [Matrix a] -> Matrix a
accumulate = foldr (<*>) (i 4)

-- Testing
test example = (accumulate $ (map dh parameters) `applyList` example) <*> tool
