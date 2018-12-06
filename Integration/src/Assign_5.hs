{- 
 - Name: Benjamin Kostiuk
 - Date: 12/01/2018
 -}
module Assign_5 where

{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: Computes an approximation of the value of a definite integral on a region [a,b]
   of a function g with n partitions.
   foldr is used to sum the partitions of the function, where each partition is calculated using
   the trapezoidal rule. The width of each partition is computed as dx.
   The indexcies of the partitions are generated from a list comprehension as (a + i*dx) where 1 <= i <= n
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
  | (n <= 0) = error "definiteIntegral only defined on positive partitions."
  | otherwise = foldr (\x xs -> (g (x - dx) + g x) / 2 * dx + xs) 0 [a + dx * i | i <- [1..(fromIntegral n)]]
      where 
        dx = (b - a) / (fromIntegral n)

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: Computes an approximation of the area between the curves x^(1/n) and x^n
   on the interval [0,1] with 1000 partitions.
   The area between the curves is defined using the definiteIntegral function as the 
   integral of x^(1/n) from [0,1] - the integral of x^n from [1,0].
   The ** function is used instead of ^ because (1/n) results in a fractional.
 -}
funH :: Integer -> Double
funH n = if n <= 0 then error "funH only defined on positive integers." 
                   else definiteIntegral 0 1 (\x -> x**(1 / fromIntegral n)) 1000 - definiteIntegral 0 1 (\x -> x^n) 1000

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: Computes an approximation of the area under the curve n^x
   on the interval [-1,1] with 1000 partitions.
   The area is defined using the definiteIntegral function. 
   The (**) function is used because x in the lambda expression is a Double.
 -}
funK :: Double -> Double
funK n = if n <= 0 then error "funK only defined on postive numbers." 
                   else definiteIntegral (-1) 1 (\x -> n**x) 1000

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 0.0 0.0 (\x -> 0) 0
 - - Expected Output: error
 - - Acutal Output: *** Exception: definiteIntegral only defined on positive partitions.
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 0.0 0.0 (\x -> 0) 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: definiteIntegral 
 - - Test Case Number:
 - - Input: 0.0 1.0 (\x -> 1) 1
 - - Expected Output: 1.0
 - - Acutal Output: 1.0
 - -----------------------------------------------------------------
 - - Function: definiteIntegral 
 - - Test Case Number:
 - - Input: 0.0 1.0 (\x -> 1) 5
 - - Expected Output: 1.0
 - - Acutal Output: 1.0
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: (-1.5) (1.0) (\x -> 1) 1
 - - Expected Output: 2.5
 - - Acutal Output: 2.5
 - -----------------------------------------------------------------
 - - Function: definiteIntegral 
 - - Test Case Number:
 - - Input: 0.0 (-3.0) (\x -> x) 1
 - - Expected Output: 4.5
 - - Acutal Output: 4.5
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: (-3.0) 3.0 (\x -> x) 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: definiteIntegral 
 - - Test Case Number:
 - - Input: 0.0 4.0 (\x -> x^2) 1
 - - Expected Output: 32.0
 - - Acutal Output: 32.0
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 0.0 4.0 (\x -> x^2) 10
 - - Expected Output: 21.4
 - - Acutal Output: 21.44000000000001
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 0.0 4.0 (\x -> x^2) 100
 - - Expected Output: 21.3333
 - - Acutal Output: 21.3344
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: (-2.0) (-1.99) (\x -> x^4) 100
 - - Expected Output: 0.158408
 - - Acutal Output: 0.15840798041800344
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 1.0 4.0 (\x -> (1/x)) 100
 - - Expected Output: 1.3863
 - - Acutal Output: 1.3863646668991465
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 0.0 (pi/2) (\x -> sin x) 100
 - - Expected Output: 1.0
 - - Acutal Output: 0.9999794382396078
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: (-2*pi) (2*pi) (\x -> cos x) 100
 - - Expected Output: 0.0
 - - Acutal Output: 1.0824674490095276e-15
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 1.0 7.0 (\x -> log x) 100
 - - Expected Output: 7.6214
 - - Acutal Output: 7.621113936388163
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: (-1.0) (-7.0) (\x -> log x) 100
 - - Expected Output: NaN
 - - Acutal Output: NaN
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:
 - - Input: 0.004 71.04 (\x -> x^2 - log x + cos x) 1000
 - - Expected Output: 119275.0
 - - Acutal Output: 119274.6169754577
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number:
 - - Input: 0
 - - Expected Output: error
 - - Acutal Output: *** Exception: funH only defined on postive integers.
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 
 - - Input: (-1)
 - - Expected Output: error
 - - Acutal Output: *** Exception: funH only defined on positive integers.
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 
 - - Input: 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number:
 - - Input: 10
 - - Expected Output: 0.81818
 - - Acutal Output: 0.8179718838152743
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number:
 - - Input: 63
 - - Expected Output: 0.96875
 - - Acutal Output: 0.9683095249716419
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number:
 - - Input: 10000000
 - - Expected Output: 0.999999
 - - Acutal Output: 0.9989999004373001
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number:
 - - Input: 0
 - - Expected Output: error
 - - Acutal Output: *** Exception: funK only defined on positive numbers.
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number:
 - - Input: (-1)
 - - Expected Output: error
 - - Acutal Output: *** Exception: funK only defined on positive numbers.
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number:
 - - Input: 1.0
 - - Expected Output: 2.0
 - - Acutal Output: 2.0000000000000013
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number:
 - - Input: 2.0
 - - Expected Output: 2.1640
 - - Acutal Output: 2.164042907907025
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number:
 - - Input: 0.4
 - - Expected Output: 2.29185
 - - Acutal Output: 2.291849644071788
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number:
 - - Input: 0.000001
 - - Expected Output: 72382.4
 - - Acutal Output: 72387.0187620579
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 
 - - Input: 1000000.0
 - - Expected Output: 72382.4
 - - Acutal Output: 72387.0187620579
 - -----------------------------------------------------------------
 - End of Test Cases
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - QuickCheck Cases
 - -----------------------------------------------------------------
 - See Assign_5_Test.hs for QuickCheck propositions and cases.
 -}

