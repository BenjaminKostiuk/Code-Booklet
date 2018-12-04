{- Assignment 1
 - Name: Benjamin Kostiuk
 - Date: 9/25/2018
 -}
 -- Module for computing all roots of a polynomial of the third degree
module Assign_1_ExtraCredit where
import Data.Complex
-- see https://www.stackage.org/haddock/lts-8.24/base-4.9.1.0/Data-Complex.html

{- -----------------------------------------------------------------
 - cubicRoot
 - -----------------------------------------------------------------
 - Description: Computes the cubic root of a Double
 -}
cubicRoot :: Double -> Double
cubicRoot n
    | (n < 0) = - ((abs n) ** (1/3))
    | otherwise = n ** (1/3)

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: Calculates the value of Q from coefficients a, b and c
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (3*a*c - b^2) / (9*a^2) 

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: Calculates the value of R from coefficients a, b, c and d 
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (9*a*b*c - 27*a^2*d - 2*b^3) / (54*a^3)

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: Calculates the value of the discriminant from R and Q
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = q^3 + r^2

{- -----------------------------------------------------------------
 - cubicComplexS
 - -----------------------------------------------------------------
 - Description: Computes a complex double from the value of S from Q and R 
 - The value of S is only calculated when the discriminant >= 0 because it is not needed for discriminant < 0
 -}
cubicComplexS :: Double -> Double -> Complex Double
cubicComplexS q r
    | (disc >= 0) = cubicRoot(r + sqrt(disc)) :+ 0
    | otherwise = 0 :+ 0 -- If disc < 0, not needed for calculations --
    where
        disc = cubicDisc q r
       
{- -----------------------------------------------------------------
 - cubicComplexT
 - -----------------------------------------------------------------
 - Description: Computes a complex double from the value of T from Q and R 
 - The value of T is only calculated when the discriminant >= 0 because it is not needed for discriminant < 0
 -}
cubicComplexT :: Double -> Double -> Complex Double
cubicComplexT q r
    | (disc >= 0) = cubicRoot(r - sqrt(disc)) :+ 0
    | otherwise = 0 :+ 0 -- If disc < 0, not needed for calculations --
    where
        disc = cubicDisc q r

{- -----------------------------------------------------------------
 - cmplxRealSolutions
 - -----------------------------------------------------------------
 - Description: Computes a list of real roots from the angular components of a cubic equation 
 - Recursively returns the list of real roots in complex double form
 -}
cmplxRealSolutions :: [Complex Double] -> Double -> Double -> Double -> Double -> Double -> [Complex Double]
cmplxRealSolutions xs n a b q theta
    | (length xs == 3) = xs
    | otherwise = cmplxRealSolutions (xs++[x]) (n+2) a b q theta    -- n is incremented by two to compute all angular solutions --
    where
        x = (2 * sqrt(-q) * cos((theta / 3) + (n*pi/3)) - (b / (3*a))) :+ 0

{- -----------------------------------------------------------------
 - cubicComplexSolutions
 - -----------------------------------------------------------------
 - Description: Computes a list of complex double roots of a cubic equation 
 - If discriminant > 0, returns a list with one real and two imaginary roots
 - If discriminant equal to 0, returns a list with three roots, with x2 = x3
 - If discriminant < 0, returns a list with three distinct real roots
 -}
cubicComplexSolutions :: Double -> Double -> Double -> Double -> [Complex Double]
cubicComplexSolutions a b c d
    | ((abs disc) < 1e-9) = [x1 :+ 0, x2 :+ 0, x2 :+ 0]
    | (disc > 0)          = [x1 :+ 0, x2 :+ (((sqrt 3) / 2) * (s - t)), x2 :+ (-((sqrt 3) / 2) * (s - t))] 
    | otherwise           = cmplxRealSolutions [] 0 a b q theta
    where
        q = cubicQ a b c
        r = cubicR a b c d
        s = realPart (cubicComplexS q r)
        t = realPart (cubicComplexT q r)
        disc = cubicDisc q r
        theta = acos(r / sqrt(-q^3)) -- Angle used in angular computation -- 
        x1 = ((s + t) - (b / (3*a)))    
        x2 = (- ((s + t) / 2) - (b / (3*a)))

{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}
{-
-- See test cases from Assign_1.hs for any other function

--cubicComplexSolutions--
    Input: 3 4 2 1      
    Expected: [-1.0 :+ 0.0, -0.1666667 :+ 0.55277, -0.1666667 :+ (-0.55277)]     
    Actual: [(-1.0000000000000004) :+ 0.0,(-0.1666666666666664) :+ 0.5527707983925663,(-0.1666666666666664) :+ (-0.5527707983925663)]

    Input: 0.00033 0.00077 (-0.0002) 0.0001
    Expected: [(-2.6100218) :+ 0.0, 0.1383442795 :+ 0.31138954,  0.1383442795 :+ (-0.31138954)]
    Actual: [(-2.6100218924147884) :+ 0.0,0.1383442795407276 :+ 0.31138954838120664,0.1383442795407276 :+ (-0.31138954838120664)]

    Input: 1 (-3) 3 (-1) 
    Expected: [1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0]
    Actual: [1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0]

    Input: 1 (-5) 8 (-4)
    Expected: [1.0 :+ 0.0, 2.0 :+ 0.0, 2.0 :+ 0.0]
    Actual: [1.0 :+ 0.0,2.0 :+ 0.0,2.0 :+ 0.0]

    Input: 1 0 (-3) 0
    Expected: [0.0 :+ 0.0, 1.7320508 :+ 0.0, (-1.7320508) :+ 0.0]
    Actual: [1.7320508075688774 :+ 0.0,(-1.732050807568877) :+ 0.0,(-3.673819061467132e-16) :+ 0.0]

    Input: 1 (-5) (-2) 24
    Expected: [4.0 :+ 0.0, (-2.0) :+ 0.0, 3.0 :+ 0.0]
    Actual: [4.000000000000001 :+ 0.0,(-1.9999999999999993) :+ 0.0,2.999999999999999 :+ 0.0]

    Input: 1 0 (-7) (-6)
    Expected: [3.0 :+ 0.0, (-2.0) :+ 0.0, (-1.0) :+ 0.0]
    Actual: [3.0000000000000004 :+ 0.0,(-1.9999999999999993) :+ 0.0,(-1.0000000000000002) :+ 0.0]

    Input: (-0.004) (-0.0122) 17 3
    Expected: [63.774967 :+ 0.0, (-66.6485177) :+ 0.0, (-0.17644953) :+ 0.0]
    Actual: [63.77496726734746 :+ 0.0,(-66.64851773002562) :+ 0.0,(-0.17644953732185886) :+ 0.0]
 -}