module Main where
import Data.List


{-
  
  The following module provides a way to compute the location of a point in the world
  coordinate system, based on its position relative to the tool coordinate system of
  a robot with any number of revolute joints, as long as Denavit-Hartenberg parameters
  off all the joints are known.

  The system can be easily extended to also work with robots with prismatic and mixed
  joints, by implementing a `dh` function that returns a lambda with a variable d instead
  of theta.

  Author: Denis Torgunov <D.Torgunov@student.bradford.ac.uk>
  BSc Intelligent Systems and Robotics (13011546)

-}


-- DATATYPE DEFINITIONS --

-- The type Domain indicates the type of numbers matrices and vectors will eventually hold.
-- Double performs fast, but Data.Numbers.CReal is much more accurate.
type Domain = Double

-- A (row) vector is represented as a list
type Vector a = [a]

-- A matrix is represented by a collection of row vectors
type Matrix a = [Vector a]


-- INPUTS --

-- Parameters specific to my UB Number: 13011546
-- Set 23
(d1, a2, a3, d5, x5, y5, z5) = (150, 210, 150, 60, 0, 10, 0)


-- Full set of axis parameters, taking above into account, as a matrix of type Domain
parameters :: Matrix Domain
parameters = [ [ d1,   0, -90 ]
             , [  0,  a2,   0 ]
             , [  0,  a3,   0 ]
             , [  0,   0, -90 ]
             , [ d5,   0,   0 ] ]
    
-- Tool coordinates in TCS, as an augmented column vector
tool :: Matrix Domain
tool = [ [  x5 ]
       , [  y5 ]
       , [  z5 ]
       , [ 1.0 ] ]

-- Axis position sets
inputs :: [[Domain]]
inputs = [ [  0,   0,   0,  -90,   0 ]
         , [ 10,   0,   0,  -90,   0 ]
         , [ 20,   0,   0,  -90,   0 ]
         , [ 30,   0,   0,  -90,   0 ]
         , [  0,  10,   0,  -90,   0 ]
           
         , [  0,  20,   0,  -90,   0 ]
         , [  0,  30,   0,  -90,   0 ]
         , [  0,   0,  10,  -90,   0 ]
         , [  0,   0,  20,  -90,   0 ]
         , [  0,   0,  30,  -90,   0 ]

         , [  0,   0,   0,  -80,   0 ]
         , [  0,   0,   0,  -70,   0 ]
         , [  0,   0,   0,  -60,   0 ]
         , [  0,   0,   0,  -90,  10 ]
         , [  0,   0,   0,  -90,  20 ]

         , [  0,   0,   0,  -90,  30 ]
         , [ 10,  10,   0,  -90,   0 ]
         , [ 90,   0,   0,  -90,   0 ]
         , [  0,  90,   0,  -90,   0 ]
         , [  0,   0,  90,  -90,   0 ]

         , [  0,   0,   0,    0,   0 ]
         , [  0,   0,   0,    0,  90 ]
         , [ 10,  20,  30,  -50,  50 ]
         , [ 11,  21,  31,  -49,  51 ]
         , [ 12,  22,  32,  -48,  52 ]

         , [ 13,  23,  33,  -47,  53 ]
         , [ 14,  24,  34,  -46,  54 ]
         , [ 15,  25,  35,  -45,  55 ]
         , [ 16,  26,  36,  -44,  56 ]
         , [ 17,  27,  37,  -43,  57 ] ]

-- MATRIX OPERATIONS --

-- Dot product
(<.>)   :: Num a => Vector a -> Vector a -> a
n <.> m = sum $ zipWith (*) n m
    
-- Matrix multiplication, defined in terms of (<.>)
(<*>)   :: Num a => Matrix a -> Matrix a -> Matrix a
m <*> n = [ [ mrows <.> ncolumns | ncolumns <- (transpose n) ] | mrows <- m ]

-- Define an n-by-n identity matrix, using the Kronecker delta function
i   :: Num a => Integer -> Matrix a
i n = map (\x -> map kronecker x) positions
    where
      -- Kronecker delta function
      kronecker (i, j) | i == j = 1
                       | otherwise = 0
      positions = [[(i,j) | j <- [1 .. n]] | i <- [1 .. n]]
         
-- Since the angles in the input set are all in degrees, we introduce a function to
-- convert degrees to radians
degToRad     :: (Floating a) => a -> a
degToRad deg = deg * pi / 180


-- Denavit-Hartenberg matrix
-- Theta-variable DH-matrix. All angle parameters are taken to be expressed in degrees
dh :: Floating a => [a] -> a -> Matrix a
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
      -- Aliases for cos and sin, to make the matrix above take up less space
      c = cos
      s = sin

-- DATA COMBINATORS --

-- Applies a list of functions to a list of arguments
-- I.e. (f:fs) `applyList` (arg:args) == (f arg):(fs `applyList` args)
applyList :: [(a -> b)]  -> [a] -> [b]
applyList = zipWith ($)

-- Gets the final transform, but multiplying a given list of matrices
-- with a 4-by-4 identity matrix as the starting value
accumulate :: (Num a) => [Matrix a] -> Matrix a
accumulate = foldr (<*>) (i 4)

-- Given a list of axis parameters, return a list of theta-dependant functions/matrices
tFunctions :: [[Domain]] -> [Domain -> Matrix Domain]
tFunctions = map dh

-- Given a list of axis parameters and current values of theta, return a transformation matrix from TCS to WCS
transform             :: [[Domain]] -> [Domain] -> Matrix Domain
transform axis thetas = accumulate $ (tFunctions axis) `applyList` thetas

-- MAIN --

-- Main. See printing functions below
main :: IO ()
main = do
  -- A list of per-axis transformation-generating functions for a given axis configuration
  -- Partially applying 'transform'
  let tf = transform parameters
  -- Apply the transformation to each of the given theta configurations
  -- Produces a list of transformations from TCS to WCS for each set of thetas
  let ts = map tf inputs
  -- Multiply each of the transforms by the TCS vector
  -- Produces a list of coordinates in WCS for each of the theta configuration
  let results = map (\t -> t <*> tool) ts

  -- Print the axis parameters
  putStrLn $ underlinedTitle "Axis Parameters"
  putStrLn ""
  putStrLn $ makeTable ["d", "a", "alpha"] (map (map approxShow) parameters)


  -- Print the tool data
  putStrLn $ underlinedTitle "Tool position"
  putStrLn ""
  putStrLn $ makeTable ["x", "y", "z"] [(map show $ take 3 $ head $ transpose tool)]
  putStrLn "\nRelateive to the TCS\n"

  -- Print the axis positions
  putStrLn $ underlinedTitle "Axis Positions"
  putStrLn ""
  putStrLn (makeTable (axisHeaders (length parameters))
                          $ zipWith axisData inputs results)

  
-- PRINTING HELPERS --

underlinedTitle       :: String -> String
underlinedTitle title = title ++ "\n" ++ (map (\_ -> '=') title)

-- Pad a String with spaces at the front, to have length n
pad     :: Int -> String -> String
pad n s = replicate (n - (length s)) ' ' ++ s

-- A column width is 2 more than the longest line in the table
determineCWidth :: [String] -> Int
determineCWidth = (+2) . head . reverse . sort . map length

-- Given a list of headers, and a list of values for each line, render the data
-- as a table
makeTable :: [String] -> [[String]] -> String
makeTable headers values
    = let allFields = headers ++ (concat values)
          columnWidth = determineCWidth allFields
          headerWidth = (length headers)*columnWidth
          padLine = map (pad columnWidth)
          formatLine = foldr (++) [] . padLine
          titleLine = formatLine headers
          underline = replicate headerWidth '-'
          contentsLines = map formatLine values
          contents = foldr (\acc line -> acc ++ "\n" ++ line) [] contentsLines
      in
        titleLine ++ "\n" ++ underline ++ "\n" ++ contents

-- Display a number as a String, rounding to 4 decimal digits
approxShow :: Domain -> String
approxShow n
    = let  (q, r) = properFraction (n * 10000)
           (r',_) = properFraction (r * 10)
           q' = if r' >= 5 then (q+1) else q
      in
        show $ (fromIntegral q') / 10000.0
  
      

-- Prepare a data regarding an axis, given a list of theta values and an (augmented) column vector of x,y,z in WCS, to be displayed as part of a table
axisData :: [Domain] -> Matrix Domain -> [String]
axisData thetas wcs
    = map approxShow thetas ++ [approxShow x] ++ [approxShow y] ++ [approxShow z]
    where
      x = head $ wcs !! 0
      y = head $ wcs !! 1
      z = head $ wcs !! 2

-- Generate axis headers to be used as part of a table
axisHeaders   :: Int -> [String]
axisHeaders n = as ++ coords
    where
      coords = ["x0", "y0", "z0"]
      as = map ("axis "++) $ map show $ take n [1..]
