module Inputs where

type Domain = Double
type Vector a = [a]
type Matrix a = [Vector a]

-- Parameters specific to my UB Number: 13011546
-- Set 23
(d1, a2, a3, d5, x5, y5, z5) = (150, 210, 150, 60, 0, 10, 0)

-- Parameters for testing
--(d1, a2, a3, d5, x5, y5, z5) = (160, 100, 100, 50, 10, 0, 10)

-- Full set of axis parameters, taking above into account
parameters :: [[Domain]]
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
