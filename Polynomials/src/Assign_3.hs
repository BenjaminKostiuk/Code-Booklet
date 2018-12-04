{- Assignment 3
 - Name: Benjamin Kostiuk
 - Date: 10/26/2018
 -}
module Assign_3 where

--Data Type used for representing polynomials
data Poly a = X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving Show

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: Evaluates a Poly at a given value n.
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue X n = n
polyValue (Coef a) _ = a
polyValue (Sum a b) n = polyValue a n + polyValue b n
polyValue (Prod a b) n = polyValue a n *  polyValue b n

{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Description: Returns the degree of a given Poly.
   Note: The degree of the zero polynomial is excluded as it is considered to be undefined.
 -}
polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree (Coef _) = 0
polyDegree X = 1
polyDegree (Sum a b) = max (polyDegree a) (polyDegree b)
polyDegree (Prod a b) = polyDegree a + polyDegree b


{- -----------------------------------------------------------------
 - polyDeriv
 - -----------------------------------------------------------------
 - Description: Computes the derivative of a Poly. 
 -}
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv X = Coef 1
polyDeriv (Coef _) =  Coef 0
polyDeriv (Sum a b) = Sum (polyDeriv a) (polyDeriv b)
polyDeriv (Prod a b) = Sum (Prod (polyDeriv a) b) (Prod a (polyDeriv b))

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 101
 - - Input: (Coef 0) 0 
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 102
 - - Input: (Coef 3) 0
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 103
 - - Input: (Coef 3) 2
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 104
 - - Input: X 2
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 105
 - - Input: X (-0.5)
 - - Expected Output: -0.5
 - - Acutal Output: -0.5
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 106
 - - Input: (Sum X X) 2
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 107
 - - Input: (Sum (Coef (-2)) X) 2
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 108
 - - Input: (Sum (Coef 2) (Coef 2)) 7
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 109
 - - Input: (Prod X X) 6
 - - Expected Output: 36
 - - Acutal Output: 36
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 110
 - - Input: (Prod (Coef 0) X) 5
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 111
 - - Input: (Prod (Coef 1) X) 5
 - - Expected Output: 5
 - - Acutal Output: 5
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 112
 - - Input: (Prod (Coef 3) (Coef (-3))) 5
 - - Expected Output: -9
 - - Acutal Output: -9
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 113
 - - Input: (Prod X (Prod X (Prod X (Prod X (Coef (-3)))))) 4
 - - Expected Output: -768
 - - Acutal Output: -768
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 114
 - - Input: (Sum (Prod X (Prod X (Coef 4))) (Prod X (Prod X (Coef (-4))))) 3
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 115
 - - Input: (Prod (Sum X (Coef (-1))) (Sum X (Coef 1))) (-4)
 - - Expected Output: 15
 - - Acutal Output: 15
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 116
 - - Input: Coef 0
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 117
 - - Input: Coef 2
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 118
 - - Input: X
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 119
 - - Input: Sum (Coef 3) X
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 120
 - - Input: Prod (Coef 0) X
 - - Expected Output: Undefined
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 121
 - - Input: Prod (Coef 3) X
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 122
 - - Input: Sum (Prod X X) X
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 123
 - - Input: Prod (Sum (Coef 3) X) (Sum (Coef 3) X)
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 124
 - - Input: Sum (Prod (Prod X X) X) (Prod X (Prod X (Prod X X))) 
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 125
 - - Input: (Coef 0)
 - - Expected Output: Coef 0
 - - Acutal Output: Coef 0
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 126
 - - Input: (Coef 3)
 - - Expected Output: Coef 0
 - - Acutal Output: Coef 0
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 127
 - - Input: (Coef (-1.5))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 128
 - - Input: X
 - - Expected Output: Coef 1
 - - Acutal Output: Coef 1
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 129
 - - Input: (Sum X X)
 - - Expected Output: Coef 2
 - - Acutal Output: Sum (Coef 1) (Coef 1)
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 130
 - - Input: (Sum X (Coef 2))
 - - Expected Output: Coef 1
 - - Acutal Output: Sum (Coef 1) (Coef 0)
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 131
 - - Input: (Sum (Coef 2) (Coef (-4))) 
 - - Expected Output: Coef 0
 - - Acutal Output: Sum (Coef 0) (Coef 0)
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 132
 - - Input: (Prod X X)
 - - Expected Output: Prod (Coef 2) X
 - - Acutal Output: Sum (Prod (Coef 1) X) (Prod X (Coef 1))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 133
 - - Input: (Prod (Coef 3) X)
 - - Expected Output: Coef 3
 - - Acutal Output: Sum (Prod (Coef 0) X) (Prod (Coef 3) (Coef 1))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 134
 - - Input: (Prod (Coef 3) (Coef (-3))) 
 - - Expected Output: Coef 0
 - - Acutal Output: Sum (Prod (Coef 0) (Coef (-3))) (Prod (Coef 3) (Coef 0))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 135
 - - Input: (Sum (Prod (Coef 2) X) (Prod (Coef (-4)) (Prod X X)))
 - - Expected Output: Sum (Coef 2) (Prod (Coef (-8)) X)
 - - Acutal Output: Sum (Sum (Prod (Coef 0) X) (Prod (Coef 2) (Coef 1))) (Sum (Prod (Coef 0) (Prod X X)) (Prod (Coef (-4)) (Sum (Prod (Coef 1) X) (Prod X (Coef 1)))))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 136
 - - Input: (Prod (Sum X (Coef (-1))) (Sum X (Coef 1)))
 - - Expected Output: Prod (Coef 2) X
 - - Acutal Output: Sum (Prod (Sum (Coef 1) (Coef 0)) (Sum X (Coef 1))) (Prod (Sum X (Coef (-1))) (Sum (Coef 1) (Coef 0)))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 End of test cases
 -}

