{- Assignment 2 Extra Credit
 - Name: Benjamin Kostiuk
 - Date: 10/20/2018
 -}
 -- Module for vectors of 2nd, 3rd and 4th dimensions and their operations
module Assign_2_ExtraCredit where
import Data.List
import Data.Ord

-- Two-Dimensional vectors
newtype Vector2 a = Vector2 (a,a)
  deriving (Show,Eq)
-- Three-Dimensional vectors
newtype Vector3 a = Vector3 (a,a,a)
  deriving (Show,Eq)
-- Four-Dimensional vectors
newtype Vector4 a = Vector4 (a,a,a,a)
	deriving (Show,Eq)

class VectorSpace v where
  {- vecZero
 - Description: Null vector -}
  vecZero       :: (Num a) => v a
  {- vecSum
 - Description: Sum of two n-dimensional vectors -}
  vecSum        :: (Num a) => v a -> v a -> v a
  {- vecScalarProd
 - Description: Computes the scalar product of a number and a n-dimensional vector -}
  vecScalarProd :: (Num a) => a -> v a -> v a
  {- vecMagnitude
 - Description: Computes the magnitude of a n-dimensional vector -}
  vecMagnitude  :: (Floating a) => v a -> a
  {- vecInnerProd
 - Description: Computes the inner product of two n-dimensional vectors -}
  vecInnerProd  :: (Num a) => v a -> v a-> a

instance VectorSpace Vector2 where    -- Instance of the VectorSpace class for 2-dimensional vectors --
  vecZero = Vector2 (0,0)
  vecSum (Vector2 (x,y)) (Vector2 (x',y')) = Vector2 (x+x',y+y')
  vecScalarProd k (Vector2 (x,y)) = Vector2 (k*x, k*y)
  vecMagnitude (Vector2 (x,y)) = sqrt(x^2 + y^2)
  vecInnerProd (Vector2 (x,y)) (Vector2 (x',y')) = x*x' + y*y'

instance VectorSpace Vector3 where    -- Instance of the VectorSpace class for 3-dimensional vectors --
  vecZero = Vector3 (0,0,0)
  vecSum (Vector3 (x,y,z)) (Vector3 (x',y',z')) = Vector3 (x+x',y+y',z+z')
  vecScalarProd k (Vector3 (x,y,z)) = Vector3 (k*x, k*y, k*z)
  vecMagnitude (Vector3 (x,y,z)) = sqrt(x^2 + y^2 + z^2)
  vecInnerProd (Vector3 (x,y,z)) (Vector3 (x',y',z')) = x*x' + y*y' + z*z'

instance VectorSpace Vector4 where    -- Instance of the VectorSpace class for 4-dimensional vectors --
  vecZero = Vector4 (0,0,0,0)
  vecSum (Vector4 (x,y,z,a)) (Vector4 (x',y',z',a')) = Vector4 (x+x',y+y',z+z',a+a')
  vecScalarProd k (Vector4 (x,y,z,a)) = Vector4 (k*x, k*y, k*z, k*a)
  vecMagnitude (Vector4 (x,y,z,a)) = sqrt(x^2 + y^2 + z^2 + a^2)
  vecInnerProd (Vector4 (x,y,z,a)) (Vector4 (x',y',z',a')) = x*x' + y*y' + z*z' + a*a'

{- -----------------------------------------------------------------
 - vecF
 - -----------------------------------------------------------------
 - Description: Returns a tuple of n-dimension vectors from the given list with the smallest and largest distance 
   from the given vector.
   minimumBy and maximumBy are used to return respectively the minimum and maximum value of a list according to a 
   comparison function.
   The comparison function used is "comparing" where elements are ordered according to the provided function.
   This reduces the number of base cases required as opposed to a traditionally recursive function.
-}
vecF :: (Floating a, Ord a, VectorSpace v) => v a -> [v a] -> (v a, v a)
vecF _ [] = (vecZero, vecZero)
vecF v vs = (minimumBy (comparing distance) vs, maximumBy (comparing distance) vs)
	where
		distance v' = vecMagnitude $ vecSum v (vecScalarProd (-1) v')
			
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 101
 - - Input: (Vector2 (0,4)) (Vector2 (-1, 3.5))
 - - Expected Output: Vector2 (-1.0,7.5)
 - - Acutal Output: Vector2 (-1.0,7.5)
 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 102
 - - Input: (Vector3 (0,2,-1)) (Vector3 (2.4,-2,7))
 - - Expected Output: Vector3 (2.4,0.0,6.0)
 - - Acutal Output: Vector3 (2.4,0.0,6.0)
 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 103
 - - Input: (Vector4 (0,2,-1.2,7.5)) (Vector4 (2,-2,7.3,-7.7))
 - - Expected Output: Vector4 (2.0,0.0,6.1,-0.2)
 - - Acutal Output: Vector4 (2.0,0.0,6.1,-0.20000000000000018)
 - -----------------------------------------------------------------
 
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 104
 - - Input: 3.0 (Vector2 (0,-2.2))
 - - Expected Output: Vector2 (0.0,-6.6)
 - - Acutal Output: Vector2 (0.0,-6.6000000000000005)
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 105
 - - Input: 3.0 (Vector3 (0,-2.33,3.0))
 - - Expected Output: Vector3 (0.0,-6.99,9.0)
 - - Acutal Output: Vector3 (0.0,-6.99,9.0)
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 106
 - - Input: 3.0 (Vector4 (0,-2.33,3.0,-0.2))
 - - Expected Output: Vector4 (0.0,-6.99,9.0,-0.6)
 - - Acutal Output: Vector4 (0.0,-6.99,9.0,-0.6000000000000001)
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 107
 - - Input: (Vector2 (0.0,0.0))
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 108
 - - Input: (Vector2 (3.0,-2.1))
 - - Expected Output: 3.661967
 - - Acutal Output: 3.661966684720111
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 109
 - - Input: (Vector3 (3.0,-2.0,0.5))
 - - Expected Output: 3.640055
 - - Acutal Output: 3.640054944640259
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 110
 - - Input: (Vector4 (3.0,-2.0,0.5,0.0))
 - - Expected Output: 3.640055
 - - Acutal Output: 3.640054944640259
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 111
 - - Input: (Vector2 (0.0,0.0)) (Vector2 (3.0,-2.0))
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 112
 - - Input: (Vector2 (2.2,0.5)) (Vector2 (3.0,-2.0))
 - - Expected Output: 5.6
 - - Acutal Output: 5.6000000000000005
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 113
 - - Input: (Vector3 (2.0,0.5,-3.0)) (Vector3 (3.0,-2.0,-3.0))
 - - Expected Output: 14.0
 - - Acutal Output: 14.0
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 114
 - - Input: (Vector4 (2.0,0.5,-3.0,1.33)) (Vector4 (3.0,-2.0,-3.0,0.0))
 - - Expected Output: 14.0
 - - Acutal Output: 14.0
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 115
 - - Input: (Vector2(1.0,1.0)) []
 - - Expected Output: (Vector2 (0.0,0.0),Vector2 (0.0,0.0))
 - - Acutal Output: (Vector2 (0.0,0.0),Vector2 (0.0,0.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 116
 - - Input: (Vector3(1.0,1.0,1.0)) []
 - - Expected Output: (Vector3 (0.0,0.0,0.0),Vector3 (0.0,0.0,0.0))
 - - Acutal Output: (Vector3 (0.0,0.0,0.0),Vector3 (0.0,0.0,0.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 117
 - - Input: (Vector3(1.0,1.0,1.0)) []
 - - Expected Output: (Vector3 (0.0,0.0,0.0),Vector3 (0.0,0.0,0.0))
 - - Acutal Output: (Vector3 (0.0,0.0,0.0),Vector3 (0.0,0.0,0.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 118
 - - Input: (Vector3(1.0,1.0, 1.0)) [Vector3(1.0,2.0,3.0)]
 - - Expected Output: (Vector3 (1.0,2.0,3.0),Vector3 (1.0,2.0,3.0))
 - - Acutal Output: (Vector3 (1.0,2.0,3.0),Vector3 (1.0,2.0,3.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 119
 - - Input: (Vector4(0.0,0.0,0.0,0.0)) [Vector4(1.0,2.0,3.0,4.0)]
 - - Expected Output: (Vector4 (1.0,2.0,3.0,4.0),Vector4 (1.0,2.0,3.0,4.0))
 - - Acutal Output: (Vector4 (1.0,2.0,3.0,4.0),Vector4 (1.0,2.0,3.0,4.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 120
 - - Input: (Vector2(0.0,0.0)) [Vector2(1.0,3.0), Vector2(4.0, 6.0)]
 - - Expected Output: (Vector2 (1.0,3.0),Vector2 (4.0,6.0))
 - - Acutal Output: (Vector2 (1.0,3.0),Vector2 (4.0,6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 121
 - - Input: (Vector3(0.0,0.0,0.0)) [Vector3(4.0,5.0,6.0), Vector3(1.0,2.0,3.0)]
 - - Expected Output: (Vector3 (1.0,2.0,3.0),Vector3 (4.0,5.0,6.0))
 - - Acutal Output: (Vector3 (1.0,2.0,3.0),Vector3 (4.0,5.0,6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 122
 - - Input: (Vector2(1.0,1.0)) [Vector2(5.0,-4.0), Vector2(-4.0, 5.0)]
 - - Expected Output: (Vector2 (5.0,-4.0),Vector2 (-4.0,5.0))
 - - Acutal Output: (Vector2 (5.0,-4.0),Vector2 (-4.0,5.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 123
 - - Input: (Vector3(1.0,1.0,1.0)) [Vector3(-4.0,-5.0,-6.0), Vector3(4.0,5.0,6.0), Vector3(-1.0,-2.0,-3.0), Vector3(1.0,2.0,3.0)]
 - - Expected Output: (Vector3 (1.0,2.0,3.0),Vector3 (-4.0,-5.0,-6.0))
 - - Acutal Output: (Vector3 (1.0,2.0,3.0),Vector3 (-4.0,-5.0,-6.0))
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 124
 - - Input: (Vector3(1.0, -1.0, 1.0)) [Vector3(2.0, 12.0, 4.0), Vector3(3.13, 10.1, 5.22), Vector3(-5.2, 6.0, 0.7), Vector3(-8.0, 1.0, -10), Vector3(-9.0, -2.0, 11.11), Vector3(10.0, -4.0, 12.0), Vector3(-11.0, -6.0, 0.33), Vector3(12, 0.0, 0.0), Vector3(13.0, -10.0, 15.0), Vector3(14.0, -12.0, 16.0)]
 - - Expected Output: (Vector3 (-5.2,6.0,0.7),Vector3 (14.0,-12.0,16.0))
 - - Acutal Output: (Vector3 (-5.2,6.0,0.7),Vector3 (14.0,-12.0,16.0))
 - -----------------------------------------------------------------
-}





