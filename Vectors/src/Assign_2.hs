{- Assignment 2
 - Name: Benjamin Kostiuk
 - Date: 10/20/2018
 -}
-- Module for vectors in three dimensions and their operations
module Assign_2 where
import Data.List
import Data.Ord

type Vector = (Double,Double,Double)

{- -----------------------------------------------------------------
 - vecZero
 - -----------------------------------------------------------------
 - Description: Vector of length 0 and magnitude 0. Used for empty comparison cases of vecF.
 -}
vecZero :: Vector
vecZero = (0.0,0.0,0.0)

{- -----------------------------------------------------------------
 - vecScalarProd
 - -----------------------------------------------------------------
 - Description: Computes the scalar product of a vector and a double, used in the calculation 
   of the distance between two vectors.
 -}
vecScalarProd :: Double -> Vector -> Vector
vecScalarProd k (x,y,z) = (k*x, k*y, k*z) 

{- -----------------------------------------------------------------
 - vecSum
 - -----------------------------------------------------------------
 - Description: Computes the sum of two vectors, used in the calculation of the distance
   between two vectors.
 -}
vecSum :: Vector -> Vector -> Vector
vecSum (x,y,z) (x',y',z') = (x+x', y+y', z+z')

{- -----------------------------------------------------------------
 - vecMagnitude
 - -----------------------------------------------------------------
 - Description: Computes the magnitude of a vector, returns a double.
 -}
vecMagnitude :: Vector -> Double
vecMagnitude (x,y,z) = sqrt(x^2 + y^2 + z^2)

{- -----------------------------------------------------------------
 - vecInnerProd
 - -----------------------------------------------------------------
 - Description: Computes the inner product of two vectors, returns a double.
 -}
vecInnerProd :: Vector -> Vector -> Double
vecInnerProd (x,y,z) (x',y',z') = x*x' + y*y' + z*z'

{- -----------------------------------------------------------------
 - vecF
 - -----------------------------------------------------------------
 - Description: Returns a tuple of vectors from the given list with the smallest and largest distance 
   from the given vector.
   minimumBy and maximumBy are used to return respectively the minimum and maximum value of a list according to 
   a comparison function. The comparison function used is "comparing" where elements are compared according 
   to their evalution using the provided function.
   This reduces the number of base cases employed as opposed to a traditionally recursive approach. 
 -}
vecF :: Vector -> [Vector] -> (Vector,Vector)
vecF _ [] = (vecZero, vecZero)      -- Returns a tuple of null vectors if the list is empty --
vecF v vs = (minimumBy (comparing distance) vs, maximumBy (comparing distance) vs)  -- Values are evaluated using the distance function for comparison -- 
    where
        -- Computes the distance between the given vector v and the vector v' --
        distance v' = vecMagnitude $ vecSum v (vecScalarProd (-1) v')

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 101
 - - Input: 0 (0.0,0.0,0.0)
 - - Expected Output: (0.0,0.0,0.0)
 - - Acutal Output: (0.0,0.0,0.0)
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 102
 - - Input: 0 (1.0, 1.0, 1.0)
 - - Expected Output: (0.0,0.0,0.0)
 - - Acutal Output: (0.0,0.0,0.0)
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 103
 - - Input: 2 (1.0, 0.5, 0.333)
 - - Expected Output: (2.0,1.0,0.666)
 - - Acutal Output: (2.0,1.0,0.666)
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 104
 - - Input: (-2) (1.0, 0.5, 0.333)
 - - Expected Output: (-2.0,-1.0,-0.666)
 - - Acutal Output: (-2.0,-1.0,-0.666)
 - -----------------------------------------------------------------

 - ----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 105
 - - Input: (0.0, 0.0, 0.0) (0.0, 0.0, 0.0)
 - - Expected Output: (0.0,0.0,0.0)
 - - Acutal Output: (0.0,0.0,0.0)
 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 106
 - - Input: (1.0, -2.0, 3.0) (0.0, 0.0, 0.0)
 - - Expected Output: (1.0,-2.0,3.0)
 - - Acutal Output: (1.0,-2.0,3.0)
 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 107
 - - Input: (1.0, -0.5, 0.333) (-1.0, 0.5, -0.333)
 - - Expected Output: (0.0,0.0,0.0)
 - - Acutal Output: (0.0,0.0,0.0)
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 108
 - - Input: (0.0, 0.0, 0.0)
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 109
 - - Input: (1.0, 2.0, 3.0)
 - - Expected Output: 3.74165739
 - - Acutal Output: 3.7416573867739413
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 110
 - - Input: (-2.0, -0.5, -0.33)
 - - Expected Output: 2.08779788
 - - Acutal Output: 2.0877978829379056
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 111
 - - Input: (1.0, -3.5, 0.333) (0.0, 0.0, 0.0)
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 112
 - - Input: (1.0, 2.0, 3.0) (-1.0, -0.5, -0.33)
 - - Expected Output: -2.990
 - - Acutal Output: -2.99
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 113
 - - Input: (-1.0, -3.5, -2.0) (0.0, -3.5, -2.0)
 - - Expected Output: 16.25
 - - Acutal Output: 16.25
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 114
 - - Input: (1.0, 1.0, 1.0) []
 - - Expected Output: ((0.0,0.0,0.0),(0.0,0.0,0.0))
 - - Acutal Output: ((0.0,0.0,0.0),(0.0,0.0,0.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 115
 - - Input: (1.0, 1.0, 1.0) [(1.0, 2.0, 3.0)]
 - - Expected Output: ((1.0,2.0,3.0),(1.0,2.0,3.0))
 - - Acutal Output: ((1.0,2.0,3.0),(1.0,2.0,3.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 116
 - - Input: (0.0, 0.0, 0.0) [(1.0, 2.0, 3.0)]
 - - Expected Output: ((1.0,2.0,3.0),(1.0,2.0,3.0))
 - - Acutal Output: ((1.0,2.0,3.0),(1.0,2.0,3.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 117
 - - Input: (0.0,0.0,0.0) [(1.0, 2.0, 3.0), (4.0, 5.0, 6.0)]
 - - Expected Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - - Acutal Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 118
 - - Input: (0.0, 0.0, 0.0) [(4.0, 5.0, 6.0), (1.0, 2.0, 3.0)]
 - - Expected Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - - Acutal Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 119
 - - Input: (1.0, 1.0, 1.0) [(5.0, -4.0, 6.0), (-4.0, 5.0, -6.0)]
 - - Expected Output: ((5.0,-4.0,6.0),(-4.0,5.0,-6.0))
 - - Acutal Output: ((5.0,-4.0,6.0),(-4.0,5.0,-6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 120
 - - Input: (1.0, 1.0, 1.0) [(1.0, 2.0, 3.0), (4.0, 5.0, 6.0), (1.0, 2.0, 3.0)]
 - - Expected Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - - Acutal Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 121
 - - Input: (1.0, 1.0, 1.0) [(5.0, 4.0, 6.0), (4.0, 5.0, 6.0), (1.0, 2.0, 3.0)]
 - - Expected Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - - Acutal Output: ((1.0,2.0,3.0),(4.0,5.0,6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 122
 - - Input: (1.0, 1.0, 1.0) [(-4.0, -5.0, -6.0), (4.0, 5.0, 6.0), (-1.0, -2.0, -3.0), (1.0, 2.0, 3.0)]
 - - Expected Output: ((1.0,2.0,3.0),(-4.0,-5.0,-6.0))
 - - Acutal Output: ((1.0,2.0,3.0),(-4.0,-5.0,-6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 123
 - - Input: (1.0, -1.0, 1.0) [(2.0, 12.0, 4.0), (3.13, 10.1, 5.22), (-5.2, 6.0, 0.7), (-8.0, 1.0, -10), (-9.0, -2.0, 11.11), (10.0, -4.0, 12.0), (-11.0, -6.0, 0.33), (12, 0.0, 0.0), (13.0, -10.0, 15.0), (14.0, -12.0, 16.0)]
 - - Expected Output: ((-5.2,6.0,0.7),(14.0,-12.0,16.0))
 - - Acutal Output: ((-5.2,6.0,0.7),(14.0,-12.0,16.0))
 - -----------------------------------------------------------------
 -}

