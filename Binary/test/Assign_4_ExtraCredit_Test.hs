{- 
 - Name: Benjamin Kostiuk
 - Date: 11/18/2018
 -}

import Assign_4_ExtraCredit

import Test.QuickCheck
import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

main :: IO ()
        -- QuickCheck Tests for Assign_4_ExtraCredit
main = do print "Performing natProp Test: "
          quickCheck natProp
          do print "Performing binNatProp Test: "
          quickCheck binNatProp
          do print "Performing plusProp Test: "
          quickCheck plusProp
          do print "Performing timeProp Test: "
          quickCheck timeProp
          do print "Performing binPlusProp Test: "
          quickCheck binPlusProp
          do print "Performing binTimesProp Test: "
          quickCheck binTimesProp
          do print "Comparing execution times on unary and binary functions:"
          benchTimes
        
{- ----------------------------------------------------------------
  QuickCheck cases for Assign_4_ExtraCredit.hs
 ------------------------------------------------------------------
 -}
 
-- Implements Nat as an instance of Arbitrary for quickCheck purposes
instance Arbitrary Nat where      
    arbitrary = frequency [(1,zero), (10,succ)]
        where
            zero = pure Z
            succ = do
                x <- arbitrary
                return $ S x

-- Implements BinNat as an instance of Arbitrary for quickCheck purposes
instance Arbitrary BinNat where
    arbitrary = frequency [(1,atom), (10,comp)]
        where
            atom = do
                digit <- arbitrary
                return $ Atom digit
            comp = do
                x <- arbitrary
                digit <- arbitrary
                return $ Compound x digit

-- Implements Digit as an instance of Arbitrary to complete the instance for BinNat
instance Arbitrary Digit where
    arbitrary = oneof [one, zero]
        where
            one = pure One
            zero = pure Zero

{- -----------------------------------------------------------------
 - natToInt
 - -----------------------------------------------------------------
 - Description: Returns the value of a Nat as an Integer.
-}
natToInt :: Nat -> Integer
natToInt Z = 0
natToInt (S x) = 1 + natToInt x

{- -----------------------------------------------------------------
 - binNatToInt
 - -----------------------------------------------------------------
 - Description: Returns the value of a BinNat as an Integer.
 -}
binNatToInt :: BinNat -> Integer
binNatToInt (Atom One) = 1
binNatToInt (Atom Zero) = 0
binNatToInt (Compound c One) = 2 * binNatToInt c + 1
binNatToInt (Compound c Zero) = 2 * binNatToInt c + 0

{- ----------------------------------------------------------------
 - Function: natPrint, natParse
 - Property: show and read are inversely related
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
natProp :: Nat -> Bool
natProp nat = show (read x :: Nat) == id x
    where
        x = show nat

{- ----------------------------------------------------------------
 - Function: binNatPrint, binNatParse
 - Property: show and read are inversely related
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
binNatProp :: BinNat -> Bool
binNatProp binNat = show (read x :: BinNat) == id x
    where
        x = show binNat

{- ----------------------------------------------------------------
 - Function: plus
 - Property: Unary Addition
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
plusProp :: Nat -> Nat -> Bool
plusProp x y = natToInt (plus x y) == natToInt x + natToInt y

{- ----------------------------------------------------------------
 - Function: times
 - Property: Unary multiplication
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
timeProp :: Nat -> Nat -> Bool
timeProp x y = natToInt (time x y) == natToInt x * natToInt y

{- ----------------------------------------------------------------
 - Function: binPlus
 - Property: Binary Addition
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
binPlusProp :: BinNat -> BinNat -> Bool
binPlusProp x y = binNatToInt (binPlus x y) == binNatToInt x + binNatToInt y

{- ----------------------------------------------------------------
 - Function: binTimes
 - Property: Binary Multiplication
 - Actual Test Result: Pass
 - ----------------------------------------------------------------
 -} 
binTimesProp :: BinNat -> BinNat -> Bool
binTimesProp x y = binNatToInt (binTimes x y) == binNatToInt x * binNatToInt y


{- -----------------------------------------------------------------
 - benchTimes
 - -----------------------------------------------------------------
 - Description: Compares the execution times of unary and binary addtion
   and multiplication using the Criterion.Main package.
   Each test case is analysed using bench then the result is printed to the terminal.
 -}
 benchTimes :: IO ()
 benchTimes = defaultMain [
     bgroup "addition" [
           bench "unary:2+2" $ whnf (plus (S (S Z))) (S (S Z))
         , bench "binary:2+2" $ whnf (binPlus (Compound (Atom One) Zero)) (Compound (Atom One) Zero) 
         , bench "unary:25+25" $ whnf (plus (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))
         , bench "binary:25+25" $ whnf (binPlus (Compound (Compound (Compound (Compound (Atom One) One) Zero) Zero) One)) (Compound (Compound (Compound (Compound (Atom One) One) Zero) Zero) One)
         , bench "unary:60+60" $ whnf (plus (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         , bench "binary:60+60" $ whnf (binPlus (Compound (Compound (Compound (Compound (Compound (Atom One) One) One) One) Zero) Zero)) (Compound (Compound (Compound (Compound (Compound (Atom One) One) One) One) Zero) Zero)
     ],
     bgroup "multiplication" [
         bench "unary:2*2" $ whnf (time (S (S Z))) (S (S Z))
         , bench "binary:2*2" $ whnf (binTimes (Compound (Atom One) Zero)) (Compound (Atom One) Zero) 
         , bench "unary:25*25" $ whnf (time (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))
         , bench "binary:25*25" $ whnf (binTimes (Compound (Compound (Compound (Compound (Atom One) One) Zero) Zero) One)) (Compound (Compound (Compound (Compound (Atom One) One) Zero) Zero) One)
         , bench "unary:60*60" $ whnf (time (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         , bench "binary:60*60" $ whnf (binTimes (Compound (Compound (Compound (Compound (Compound (Atom One) One) One) One) Zero) Zero)) (Compound (Compound (Compound (Compound (Compound (Atom One) One) One) One) Zero) Zero)
     ]
  ]