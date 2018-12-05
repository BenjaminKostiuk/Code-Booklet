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
