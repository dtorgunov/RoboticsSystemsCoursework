module Main where
import Data.List

import Inputs
    
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

-- Given a list of axis parameters, return a list of theta-dependant functions/matrices
tFunctions :: [[Domain]] -> [Domain -> Matrix Domain]
tFunctions = map dh

-- Given a list of axis parameters and current values of theta, return a transformation matrix from TCS to WCS
transform :: [[Domain]] -> [Domain] -> Matrix Domain
transform axis thetas = accumulate $ (tFunctions axis) `applyList` thetas

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
-- Printing functions

underlinedTitle :: String -> String
underlinedTitle title = title ++ "\n" ++ (map (\_ -> '=') title)

-- Make a table out of a list
pad :: Int -> String -> String
pad n s = replicate (n - (length s)) ' ' ++ s

-- A column width is 2 more than the longest line in the table
determineCWidth :: [String] -> Int
determineCWidth = (+2) . head . reverse . sort . map length

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

-- Display a number as a String, rounding to 2 decimal digits
approxShow :: Domain -> String
approxShow n
    = let  (q, r) = properFraction (n * 100)
           (r',_) = properFraction (r * 10)
           q' = if r' >= 5 then (q+1) else q
      in
        show $ (fromIntegral q') / 100.0
  
      

-- Prepare a data regarding an axis, given a list of theta values and an (augmented) column vector of x,y,z in WCS
axisData :: [Domain] -> Matrix Domain -> [String]
axisData thetas wcs
    = map approxShow thetas ++ [approxShow x] ++ [approxShow y] ++ [approxShow z]
    where
      x = head $ wcs !! 0
      y = head $ wcs !! 1
      z = head $ wcs !! 2

-- Generate axis headers
axisHeaders :: Int -> [String]
axisHeaders n = as ++ coords
    where
      coords = ["x0", "y0", "z0"]
      as = map ("axis "++) $ map show $ take n [1..]
