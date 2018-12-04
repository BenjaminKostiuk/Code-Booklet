{- Assignment 1
 - Name: Benjamin Kostiuk
 - Date: 9/24/2018
 -}
-- Module for calculating the real root(s) of a polynomial of the third degree 
module Assign_1 where

{- -----------------------------------------------------------------
 - cubicRoot
 - -----------------------------------------------------------------
 - Description: Computes the cubic root of a double
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
 - cubicS
 - -----------------------------------------------------------------
 - Description: Calculates the value of S from the values of Q and R
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubicRoot(r + sqrt(q^3 + r^2))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: Calculates the value of T from the values of Q and R
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubicRoot(r - sqrt(q^3 + r^2))

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: Computes the real roots of a cubic equation from its coefficients a, b, c, and d. 
 - If the discriminant > 0, returns a list with the only real root.
 - If the discriminant equal to 0, return a list with all 3 real roots.
 - If the discriminant < 0, no solutions are returned.
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
    | ((abs disc) < 1e-7) = [x1, x2, x2] 
    | (disc > 0)          = [x1] 
    | otherwise           = []
    where 
        q = cubicQ a b c
        r = cubicR a b c d
        s = cubicS q r
        t = cubicT q r
        disc = cubicDisc q r    -- Fetches the discriminant from cubicDisc -- 
        x1 = s + t - (b / (3*a))
        x2 = - ((s + t) / 2) - (b / (3*a))

{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}
{-  
--cubicRoot--
    Input: 8        Expected: 2.0       Actual: 2.0
    Input: 4.913    Expected: 1.7       Actual: 1.7
    Input: 0        Expected: 0.0       Actual: 0.0
    Input: (-8)     Expected: -2.0      Actual: -2.0
    Input: (-4.913) Expected: -1.7      Actual: -1.7

--cubicQ--
    Input: 1 2 3            Expected: 0.5555555 Actual: 0.5555555
    Input: 0 0 0            Expected: NaN       Actual: NaN
    Input: (-2) (-4) (-6)   Expected: 0.5555555 Actual: 0.5555555

--cubicR--
    Input: 1 2 3 4              Expected: -1.296296296  Actual: -1.2962962962962963
    Input: 0 0 0 0              Expected: NaN           Actual: NaN
    Input: (-2) (-3) (-5) (-7)  Expected: -1.25         Actual: -1.25

--cubicDisc--
    Input: 2 3      Expected: 17    Actual: 17.0
    Input: 0 0      Expected: 0     Actual: 0.0
    Input: (-4) 5   Expected: -39   Actual: -39.0
    
--cubicS--
    Input: 3 2      Expected: 1.963311454       Actual: 1.9633114542577068
    Input: 7 (-3)   Expected: 2.507267548       Actual: 2.507267548055002
    Input: 0.5 0.3  Expected: 0.914051462       Actual: 0.9140514620263525
    Input: (-2) 4   Expected: 1.897172815       Actual: 1.8971728150867002
    Input: (-6) 3   Expected: NaN               Actual: NaN
    Input: (-6.5) 8 Expected: NaN               Actual: NaN

--cubicT--
    Input: 3 2      Expected: -1.52803061       Actual: -1.5280306104739996
    Input: 7 (-3)   Expected: -2.79188394       Actual: -2.7918839397216333
    Input: 0.5 0.3  Expected: -0.547015153      Actual: -0.54701515261685
    Input: (-2) 4   Expected: 1.054200221       Actual: 1.0542002205047412
    Input: (-6) 3   Expected: NaN               Actual: NaN
    Input: (-6.5) 8 Expected: NaN               Actual: NaN

--cubicRealSolutions--
    Input: 1 1 1 (-3)           Expected: [1.0]             Actual: [0.9999999999999984]
    Input: 1 1 1 1              Expected: [-1.0]            Actual: [-1.0]
    Input: 1 0 6 (-20)          Expected: [2.0]             Actual: [1.9999999999999996]
    Input: 3 4 3 2              Expected: [-1.0]            Actual: [-1.0]
    Input: 1 2 4 8              Expected: [-2.0]            Actual: [-2.0]
    Input: (-2) 3 (-7) (-3)     Expected: [-0.35978745]     Actual: [-0.35978745038078885]
    Input: (-6) 5 3 (-12)       Expected: [-1.14102881]     Actual: [-1.1410288172617848]
    Input: 0.5 (-3) 0.2 (-12)   Expected: [6.505586541]     Actual: [6.505586541163334]

    Input: 1 (-3) 3 (-1)        Expected: [1.0, 1.0, 1.0]   Actual: [1.0,1.0,1.0]
    Input: 1 (-5) 8 (-4)        Expected: [1.0, 2.0, 2.0]   Actual: [1.0,2.0,2.0]
    Input: 1 0 0 0              Expected: [0.0, 0.0, 0.0]   Actual: [0.0,-0.0,-0.0]

    Input: 1 0 (-3) 0       Expected: []                Actual: []
    Input: 1 (-5) (-2) 24   Expected: []                Actual: []
    Input: 1 0 (-7) (-6)    Expected: []                Actual: []
-}
