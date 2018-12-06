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
