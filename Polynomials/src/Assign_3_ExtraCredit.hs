{-
 - Name: Benjamin Kostiuk
 - Date: 10/31/2018
 -}
module Assign_3_ExtraCredit where
import Assign_3

--Alternative Data Type for representing polynomials 
data PolyAlt a = Monomial a Integer
  | SumAlt (PolyAlt a) (PolyAlt a)
  deriving Show

{- -----------------------------------------------------------------
 - polyAltValue
 - -----------------------------------------------------------------
 - Description: Evaluates a PolyAlt at a given number n.
 -}
polyAltValue :: Num a => PolyAlt a -> a -> a
polyAltValue (Monomial c e) n = c * n ^ (abs e)
polyAltValue (SumAlt x y) n = polyAltValue x n + polyAltValue y n

{- -----------------------------------------------------------------
 - polyAltDegree
 - -----------------------------------------------------------------
 - Description: Returns the degree of a given PolyAlt.
   Note: The degree of the zero PolyAlt (zero polynomial) is excluded as it is considered to be undefined. 
 -}
polyAltDegree :: (Num a, Eq a) => PolyAlt a -> Integer
polyAltDegree (Monomial c e) = abs e
polyAltDegree (SumAlt a b) = max (polyAltDegree a) (polyAltDegree b)

{- -----------------------------------------------------------------
 - polyAltDeriv
 - -----------------------------------------------------------------
 - Description: Computes the derivative of a given PolyAlt.
 -}
polyAltDeriv :: Num a => PolyAlt a -> PolyAlt a
polyAltDeriv (Monomial c 0) = Monomial 0 0
polyAltDeriv (Monomial c e) = Monomial (c * fromInteger (abs e)) ((abs e) - 1) 
polyAltDeriv (SumAlt x y) = SumAlt (polyAltDeriv x) (polyAltDeriv y)

{- -----------------------------------------------------------------
 - polyAltProd
 - -----------------------------------------------------------------
 - Description: Computes the PolyAlt resulting from the product of two given PolyAlts.
 -}
polyAltProd :: Num a => PolyAlt a -> PolyAlt a -> PolyAlt a
polyAltProd (Monomial c e) (Monomial c' e') = Monomial (c*c') (abs e + abs e')
polyAltProd x (SumAlt a b) = SumAlt (polyAltProd x a) (polyAltProd x b)
polyAltProd (SumAlt a b) x = SumAlt (polyAltProd x a) (polyAltProd x b)

{- -----------------------------------------------------------------
 - polyAltProd
 - -----------------------------------------------------------------
 - Description: Computes an approximate solution for the equation PolyAlt p = 0,
   with an inital given seed s, for a given tolerance of t.
   Thus, the polyAltValue at that solution will be inferior to the tolerance t.
 -}
polyAltNewton :: (Fractional a, Ord a) => PolyAlt a -> a -> a -> a
polyAltNewton p s t
    | abs (polyAltValue p s) < t = s
    | otherwise = polyAltNewton p s' t
      where
        s' = s - (polyAltValue p s) / (polyAltValue (polyAltDeriv p) s)

{- -----------------------------------------------------------------
 - polyToPolyAlt
 - -----------------------------------------------------------------
 - Description: Converts a given Poly to a PolyAlt.
 -}
polyToPolyAlt :: (Num a, Eq a) => Poly a -> PolyAlt a
polyToPolyAlt X = Monomial 1 1
polyToPolyAlt (Coef x) = Monomial x 0
polyToPolyAlt (Sum x y) = SumAlt (polyToPolyAlt x) (polyToPolyAlt y)
polyToPolyAlt (Prod x y) = polyAltProd (polyToPolyAlt x) (polyToPolyAlt y)

{- -----------------------------------------------------------------
 - polyAltToPoly
 - -----------------------------------------------------------------
 - Description: Converts a given PolyAlt to a Poly.
 -}
polyAltToPoly :: (Num a, Eq a) => PolyAlt a -> Poly a
polyAltToPoly (Monomial c 0) = Coef c
polyAltToPoly (Monomial c e) = Prod (polyAltToPoly (Monomial c ((abs e) - 1))) X
polyAltToPoly (SumAlt x y) = Sum (polyAltToPoly x) (polyAltToPoly y)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 101
 - - Input: (Monomial 0 0) 0
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 102
 - - Input: (Monomial 0 2) 2
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 103
 - - Input: (Monomial 2 3) 5
 - - Expected Output: 250
 - - Acutal Output: 250
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 104
 - - Input: (Monomial 2 (-3)) 5
 - - Expected Output: 250
 - - Acutal Output: 250
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 105
 - - Input: (Monomial (-1.5) (-2)) 4
 - - Expected Output: -24.0
 - - Acutal Output: -24.0
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 106
 - - Input: (SumAlt (Monomial 3 1) (Monomial (-3) 1)) 3
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 107
 - - Input: (SumAlt (Monomial (-7) (-2)) (Monomial 12 2)) 3
 - - Expected Output: 45
 - - Acutal Output: 45
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 108
 - - Input: (SumAlt (SumAlt (Monomial 2 (-4)) (Monomial (-3) 4)) (SumAlt (Monomial 0 9) (Monomial 3 5))) 5
 - - Expected Output: 8750
 - - Acutal Output: 8750
 - -----------------------------------------------------------------
  - -----------------------------------------------------------------
 - - Function: polyAltDegree
 - - Test Case Number: 109
 - - Input: (Monomial 3 0)
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyAltDegree 
 - - Test Case Number: 110
 - - Input: (Monomial 4 2)
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyAltDegree
 - - Test Case Number: 111
 - - Input: (Monomial 7 (-8))
 - - Expected Output: 8
 - - Acutal Output: 8
 - -----------------------------------------------------------------
 - - Function: polyAltDegree
 - - Test Case Number: 112
 - - Input: (SumAlt (Monomial 2 3) (Monomial 2 (-3)))
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - - Function: polyAltDegree
 - - Test Case Number: 113
 - - Input: (SumAlt (Monomial 2 3) (Monomial 2 (-7)))
 - - Expected Output: 7
 - - Acutal Output: 7
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltDeriv
 - - Test Case Number: 114
 - - Input: (Monomial 0 0)
 - - Expected Output: Monomial 0 0
 - - Acutal Output: Monomial 0 0
 - -----------------------------------------------------------------
 - - Function: polyAltDeriv
 - - Test Case Number: 115
 - - Input: (Monomial 3 0)
 - - Expected Output: Monomial 0 0
 - - Acutal Output: Monomial 0 0
 - -----------------------------------------------------------------
 - - Function: polyAltDeriv
 - - Test Case Number: 116
 - - Input: (Monomial 2 (-7))
 - - Expected Output: Monomial 14 6
 - - Acutal Output: Monomial 14 6
 - -----------------------------------------------------------------
 - - Function: polyAltDeriv
 - - Test Case Number: 117
 - - Input: (SumAlt (Monomial (-3) (-4)) (Monomial 12 0))
 - - Expected Output: SumAlt (Monomial (-12) 3) (Monomial 0 0)
 - - Acutal Output: SumAlt (Monomial (-12) 3) (Monomial 0 0)
 - -----------------------------------------------------------------
  - -----------------------------------------------------------------
 - - Function: polyAltProd
 - - Test Case Number: 118
 - - Input: (Monomial 0 0) (Monomial 0 0)
 - - Expected Output: Monomial 0 0
 - - Acutal Output: Monomial 0 0
 - -----------------------------------------------------------------
 - - Function: polyAltProd
 - - Test Case Number: 119
 - - Input: (Monomial 3 (-4)) (Monomial (-2) 0)
 - - Expected Output: Monomial (-6) 4
 - - Acutal Output: Monomial (-6) 4
 - -----------------------------------------------------------------
 - - Function: polyAltProd
 - - Test Case Number: 120
 - - Input: (SumAlt (Monomial 2 3) (Monomial (-1) (-4))) (SumAlt (Monomial 3 0) (Monomial (-4) (-6)))
 - - Expected Output: SumAlt (SumAlt (Monomial 6 3) (Monomial (-3) 4)) (SumAlt (Monomial (-8) 9) (Monomial 4 10))
 - - Acutal Output: SumAlt (SumAlt (Monomial 6 3) (Monomial (-3) 4)) (SumAlt (Monomial (-8) 9) (Monomial 4 10))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltNewton
 - - Test Case Number: 121
 - - Input: (SumAlt (Monomial 1 (-3)) (Monomial (-7) 0)) 1.92 0.005
 - - Expected Output: 1.913
 - - Acutal Output: 1.9129571759259258
 - -----------------------------------------------------------------
 - - Function: polyAltNewton
 - - Test Case Number: 122
 - - Input: (SumAlt (Monomial (-2) (-3)) (SumAlt (Monomial 5 2) (Monomial (-3) 0))) (1.66) (0.005)
 - - Expected Output: -0.686 or 1.0, or 2.186
 - - Acutal Output: -0.6861500086816795
 - -----------------------------------------------------------------
 - - Function: polyAltNewton
 - - Test Case Number: 123
 - - Input: (SumAlt (Monomial (0.6) (-3)) (Monomial (-1) 1)) (-0.3) 0.000005
 - - Expected Output: 0.0
 - - Acutal Output: 4.0359370305411563e-13
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyToPolyAlt
 - - Test Case Number: 124
 - - Input: (Coef 3)
 - - Expected Output: Monomial 3 0
 - - Acutal Output: Monomial 3 0
 - -----------------------------------------------------------------
 - - Function: polyToPolyAlt
 - - Test Case Number: 125
 - - Input: (Sum X (Sum (Coef (-2)) X))
 - - Expected Output: SumAlt (Monomial 1 1) (SumAlt (Monomial (-2) 0) (Monomial 1 1))
 - - Acutal Output: SumAlt (Monomial 1 1) (SumAlt (Monomial (-2) 0) (Monomial 1 1))
 - -----------------------------------------------------------------
 - - Function: polyToPolyAlt
 - - Test Case Number: 126
 - - Input: (Sum (Prod X X) (Prod (Coef 3) (Prod X (Prod X X))))
 - - Expected Output: SumAlt (Monomial 1 2) (Monomial 3 3)
 - - Acutal Output: SumAlt (Monomial 1 2) (Monomial 3 3)
 - -----------------------------------------------------------------
 - - Function: polyToPolyAlt
 - - Test Case Number: 127
 - - Input: (Prod (Sum X (Coef 1)) (Sum X (Coef (-1))))
 - - Expected Output: SumAlt (SumAlt (Monomial 1 2) (Monomial 1 1)) (SumAlt (Monomial (-1) 1) (Monomial (-1) 0))
 - - Acutal Output: SumAlt (SumAlt (Monomial 1 2) (Monomial 1 1)) (SumAlt (Monomial (-1) 1) (Monomial (-1) 0))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 128
 - - Input: (Monomial 3 0)
 - - Expected Output: Coef 3
 - - Acutal Output: Coef 3
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 129
 - - Input: (Monomial (-4) 1)
 - - Expected Output: Prod (Coef (-4)) X
 - - Acutal Output: Prod (Coef (-4)) X
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 130
 - - Input: (Monomial 7 5)
 - - Expected Output: Prod (Prod (Prod (Prod (Prod (Coef 7) X) X) X) X) X
 - - Acutal Output: Prod (Prod (Prod (Prod (Prod (Coef 7) X) X) X) X) X
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 131
 - - Input: (SumAlt (Monomial 3 0) (Monomial (-4) 0))
 - - Expected Output: Sum (Coef 3) (Coef (-4))
 - - Acutal Output: Sum (Coef 3) (Coef (-4))
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 132
 - - Input: (SumAlt (Monomial 3 (-2)) (Monomial (-4) 1))
 - - Expected Output: Sum (Prod (Prod (Coef 3) X) X) (Prod (Coef (-4)) X)
 - - Acutal Output: Sum (Prod (Prod (Coef 3) X) X) (Prod (Coef (-4)) X)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
   End of test cases
 -}