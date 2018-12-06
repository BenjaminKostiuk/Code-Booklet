{- 
 - Name: Benjamin Kostiuk
 - Date: 12/02/2018
 -}

import Assign_5
import Test.QuickCheck

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck integralProp1
          do print "Performing Test 2: "
          quickCheck integralProp2
          do print "Performing Test 3: "
          quickCheck integralProp3
          do print "Performing Test 4: "
          quickCheck integralProp4
          do print "Performing Test 5: "
          quickCheck funHProp1
          do print "Performing Test 6: "
          quickCheck funHProp2
          do print "Performing Test 7: "
          quickCheck funKProp1
          do print "Performing Test 8: "
          quickCheck funKProp2
          do print "Performing Test 9: "
          quickCheck funKProp3

{-
 - Function: definiteIntegral
 - Property: The integral of f from [a,b] is the inverse of the integral of f from [b,a]
 - Actual Test Result: Pass
 -}
integralProp1 :: Double -> Double -> Integer -> Bool
integralProp1 a b n = (not (n > 0)) || abs (definiteIntegral a b (\x -> x^2) n + definiteIntegral b a (\x -> x^2) n) < 1e-3

{-
 - Function: definiteIntegral
 - Property: The integral from [a,b] of f(x) = 1 is equal to b - a
 - Actual Test Result: Pass
 -}
integralProp2 :: Double -> Double -> Integer -> Bool
integralProp2 a b n = (not (n > 0)) || abs (definiteIntegral a b (\x -> 1) n - (b - a)) < 1e-7

{-
 - Function: definiteIntegral
 - Property: The integral from [a,a] is 0
 - Actual Test Result: Pass
 -}
integralProp3 :: Double -> Integer -> Bool
integralProp3 a n = (not (n > 0)) || abs (definiteIntegral a a (\x -> x^4 - 3*x + 2) n) < 1e-7

{-
 - Function: definiteIntegral
 - Property: The integral from [a,b] of c*f(x) is equal to c * the integral from [a,b] of f(x)
 - Actual Test Result: Pass
 -}
integralProp4 :: Double -> Double -> Integer -> Double -> Bool
integralProp4 a b n c = (not (n > 0)) || abs (definiteIntegral a b (\x -> c * x^2) n - c * definiteIntegral a b (\x -> x^2) n) < 1e-3

{-
 - Function: funH
 - Property: The area between the curves x^n and x^(1/n) will always 
   be greater than or equal to 0 and less than 1.
 - Actual Test Result: Pass 
 -}
funHProp1 :: Integer -> Bool
funHProp1 n = (not (n > 0)) || funH n >= 0 && funH n < 1

{-
 - Function: funH
 - Property: If m > n then the area between the curves x^m and x^(1/m) will always be greater than
   the area between the curves x^n and x^(1/n).
 - Actual Test Result: Pass
 -}
funHProp2 :: Integer -> Bool
funHProp2 n = (not (n > 0)) || funH n < funH (n + 1)

{-
 - Function: funK
 - Property: The integral of n^x from [-1,1] will always be positive
 - Actual Test Result: Pass
 -}
funKProp1 :: Double -> Bool
funKProp1 n = (not (n > 0)) || funK n > 0

{-
 - Function: funK
 - Property: If n & m >= 1 and m > n then m^x will always have a greater integral than n^x.
 - Actual Test Result: Pass
 -}
funKProp2 :: Double -> Bool
funKProp2 n = (not (n >= 1)) || funK n < funK (n + 1)

{-
 - Function: funK
 - Property: The integral of n^x and (1/n)^x from [-1,1] are always equal
 - Actual Test Result: Pass
 -}
funKProp3 :: Double -> Bool
funKProp3 n = (not (n > 0)) || abs (funK n - funK (1/n)) < 1e-7




