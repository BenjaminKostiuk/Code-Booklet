{- QuickCheck testing
 - Benjamin Kostiuk
 - Date: 10/26/2018
-}

import Assign_3
import Assign_3_ExtraCredit

main :: IO ()
            -- QuickCheck Tests for Assign_4
main = do print "Performing polyListValue Test 1: "
          quickCheck polyListValueProp
          do print "Performing polyListDegree Test 1: "
          quickCheck polyListDegreeProp
          do print "Performing polyListDeriv Test 1: "
          quickCheck polyListDerivProp
          do print "Performing polyListSum Test 1: "
          quickCheck polyListSumProp1
          do print "Performing polyListSum Test 2: "
          quickCheck polyListSumProp2
          do print "Performing polyListSum Test 3: "
          quickCheck polyListSumProp3
          do print "Performing polyListProd Test 1: "
          quickCheck polyListProdProp1
          do print "Performing polyListProd Test 2: "
          quickCheck polyListProdProp2
          do print "Performing polyListProd Test 3: "
          quickCheck polyListProdProp3

{- ----------------------------------------------------------------
  QuicCheck Cases for Assign_3
 - ----------------------------------------------------------------
 - Function: polyListValue
 - Property: You can split a list of coefficients in two, replace the lost coefficients
   with zeros and evaluated the two to produce the same result.
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -}
polyListValueProp :: [Integer] -> Integer -> Bool
polyListValueProp [] n = polyListValue (PolyList []) n == 0
polyListValueProp (x:xs) n = polyListValue (PolyList (x:xs)) n == (polyListValue (PolyList [x]) n + polyListValue (PolyList (0:xs)) n)

{- ----------------------------------------------------------------
 - Function: polyListDegree
 - Property: The degree of a PolyList is related to its length, thus 
   PolyLists with same lengths should have the same degree.
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -}
polyListDegreeProp :: [Integer] -> [Integer] -> Bool
polyListDegreeProp xs ys
  | (xs == [] || ys == []) = True
  | (length xs > length ys) = polyListDegree (PolyList xs) > polyListDegree (PolyList ys)
  | (length xs < length ys) = polyListDegree (PolyList xs) < polyListDegree (PolyList ys)
  | otherwise = polyListDegree (PolyList xs) == polyListDegree (PolyList ys)

{- ----------------------------------------------------------------
 - Function: polyListDeriv
 - Property: The degree of f is always superior or equal to its derivative f'
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
polyListDerivProp :: [Integer] -> Bool
polyListDerivProp xs = not (length xs > 1) || polyListDegree (polyListDeriv (PolyList xs)) < polyListDegree (PolyList xs)

{- ----------------------------------------------------------------
 - Function: polyListSum
 - Property: a + b = b + a (Commutativity of addtion)
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
polyListSumProp1 :: [Integer] -> [Integer] -> Integer -> Bool
polyListSumProp1 xs ys n = polyListValue (polyListSum (PolyList xs) (PolyList ys)) n == polyListValue (polyListSum (PolyList ys) (PolyList xs)) n

{- ----------------------------------------------------------------
 - Function: polyListSum
 - Property: a + 0 = a (Additive identitiy element)
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -}
polyListSumProp2 :: [Integer] -> Integer -> Bool
polyListSumProp2 xs n = polyListValue (polyListSum (PolyList xs) (PolyList [])) n ==  polyListValue (PolyList xs) n

{- ----------------------------------------------------------------
 - Function: polyListSum
 - Property: a - a = 0 (Negation of a polynomial)
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -}
polyListSumProp3 :: [Integer] -> Integer -> Bool
polyListSumProp3 xs n = polyListValue (polyListSum (PolyList xs) (PolyList (map (*(-1)) xs))) n == 0

{- ----------------------------------------------------------------
 - Function: polyListProd
 - Property: a * b = b * a (Commutativity of multiplication)
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -}
polyListProdProp1 :: [Integer] -> [Integer] -> Integer -> Bool
polyListProdProp1 xs ys n = polyListValue (polyListProd (PolyList xs) (PolyList ys)) n == polyListValue (polyListProd (PolyList ys) (PolyList xs)) n

{- ----------------------------------------------------------------
 - Function: polyListProd
 - Property: a * 1 = a (Multiplicative identity element)
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -}
polyListProdProp2 :: [Integer] -> Integer -> Bool
polyListProdProp2 xs n = polyListValue (polyListProd (PolyList xs) (PolyList [1])) n == polyListValue (PolyList xs) n

{- ----------------------------------------------------------------
 - Function: polyListProd
 - Property: (a * b) * c = (a * c) * b (Associativity of multiplication)
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -}
polyListProdProp3 :: [Integer] -> [Integer] -> [Integer] -> Integer -> Bool
polyListProdProp3 xs ys zs n = polyListValue (polyListProd (PolyList xs) (polyListProd (PolyList ys) (PolyList zs))) n == polyListValue (polyListProd (PolyList zs) (polyListProd (PolyList ys) (PolyList xs))) n 

{- ----------------------------------------------------------------
  End of QuickCheck cases for Assign_3.hs
 -------------------------------------------------------------------}