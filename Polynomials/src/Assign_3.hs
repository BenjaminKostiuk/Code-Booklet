{-
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

--Data Type for representing polynomials as lists of coefficients
newtype PolyList a = PolyList [a] deriving Show

{- -----------------------------------------------------------------
 - getPolyList
 - -----------------------------------------------------------------
 - Description: Reads the coefficients of a polynomial in standard form from a file, where
   each line corresponds to a coefficient and returns a PolyList of Integers.
   map is used together with lines in order to read the values in the file and transform them into a list of Integers.
 -}
getPolyList :: FilePath -> IO (PolyList Integer)
getPolyList file = do 
    input <- readFile file
    return $ PolyList $ map read (lines input)

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: Evaluates a PolyList at a certain value n using Horner's method,
   reducing computation compared to traditional evaluation.
 -}
polyListValue :: Num a => PolyList a -> a -> a
polyListValue (PolyList []) _ = 0
polyListValue (PolyList (x:xs)) n = x + n * (polyListValue (PolyList xs) n)

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
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: Returns the degree of a given PolyList.
   Length is used because the degree of a polynomial in the standard form can 
   be defined as its length - 1.
   The degree of the zero polynomial is not included as it is undefined. 
 -}
polyListDegree :: (Num a, Eq a) => PolyList a -> Integer
polyListDegree (PolyList (x:xs)) = toInteger $ length (x:xs) - 1

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
 - polyListDeriv
 - -----------------------------------------------------------------
 - Description: Returns a PolyList representing the derivative of a given PolyList.
   foldl recursively multiplies from left to right coefficients with their degree (also equal to 
   the number of preceding coefficients).
 -}
polyListDeriv :: (Num a, Eq a) => PolyList a -> PolyList a
polyListDeriv (PolyList []) = PolyList []
polyListDeriv (PolyList [x]) = PolyList []
polyListDeriv (PolyList (x:xs)) = PolyList $ foldl (\b a -> b ++ [a * (fromIntegral (length b + 1))]) [] xs

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
 - polyListSum
 - -----------------------------------------------------------------
 - Description: Returns a PolyList representing the sum of two PolyLists.
   Coefficients with the same degree are added together using the zipWith function,
   and the remainder of the longer list is concatenated.
 -}
polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList xs) (PolyList ys)
    | (length xs > length ys) = PolyList $ (zipWith (+) xs ys) ++ drop (length ys) xs
    | (length xs < length ys) = PolyList $ (zipWith (+) xs ys) ++ drop (length xs) ys
    | otherwise = PolyList $ trim $ zipWith (+) xs ys
        where
            -- Trims all trailing zeros from a list of coefficients.
            -- Uses viewPatterns to pattern match with the last element of the list.
            trim :: (Num a, Eq a) => [a] -> [a]
            trim [] = []
            trim (reverse -> x:(reverse -> xs)) = if x == 0 then trim xs else xs ++ [x]   
            
{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: Returns a PolyList that represents the product of two given PolyLists.
   The (PolyList ys) is multipled by each coefficient in PolyList xs using map, then
   added together using polyListSum. 
 -}
polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) _ = PolyList []
polyListProd _ (PolyList []) = PolyList []
polyListProd (PolyList (x:xs)) (PolyList ys) = polyListSum (PolyList (map (*x) ys)) (polyListProd (PolyList xs) (PolyList (0:ys)))

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: Converts a PolyList to a polynomial of the form (Poly a).
   Represents the list of coefficients as a Poly of the form used in 
   Horner's method. Recursively takes the first element of the PolyList and 
   adds its coefficient with the product of X and the rest of the PolyList.
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList []) = Coef 0
polyListToPoly (PolyList [x]) = Coef x
polyListToPoly (PolyList (x:xs)) = Sum (Coef x) (Prod (polyListToPoly (PolyList xs)) X)

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: Converts a Poly to a PolyList of the form (PolyList a).
   Recursively converts a Poly in bits then computes the PolyList using 
   polyListSum and polyListProd.
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList (Coef 0) = PolyList []
polyToPolyList (Coef x) = PolyList [x]
polyToPolyList X = PolyList [0,1]
polyToPolyList (Sum a b) = polyListSum (polyToPolyList a) (polyToPolyList b)
polyToPolyList (Prod a b) = polyListProd (polyToPolyList a) (polyToPolyList b)