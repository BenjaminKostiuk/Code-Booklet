{-# LANGUAGE ViewPatterns #-}
{- 
 - Name: Benjamin Kostiuk
 - Date: 11/18/2018
 -}
module Assign_4_ExtraCredit where
import Criterion.Main

--Data Type representing natural numbers
data Nat = Z
         | S Nat

data Digit = Zero
           | One deriving Show

--Data Type representing binary numbers         
data BinNat = Atom Digit
            | Compound BinNat Digit 

{- -----------------------------------------------------------------
 - natPrint
 - -----------------------------------------------------------------
 - Description: Returns a string representing a given Nat with the prefix "Nat:"
 -}
natPrint :: Nat -> String
natPrint n = "Nat:" ++ print n
    where
        print :: Nat -> String
        print Z = "0"
        print (S x) = "S" ++ print x

instance Show Nat where     -- Implements Nat as part of the Show Class using the natPrint function
    show n = natPrint n

{- -----------------------------------------------------------------
 - binNatPrint
 - -----------------------------------------------------------------
 - Description: Returns a string representing a given BinNat with the prefix "BinNat:"
 -}
binNatPrint :: BinNat -> String
binNatPrint n = "BinNat:" ++ print n
    where
        print :: BinNat -> String
        print (Atom Zero) = "0"
        print (Atom One) = "1"
        print (Compound b Zero) = print b ++ "0"
        print (Compound b One) = print b ++ "1"

instance Show BinNat where      -- Implements BinNat as part of the Show Class using the binNatPrint function 
    show n = binNatPrint n

{- -----------------------------------------------------------------
 - natParse
 - -----------------------------------------------------------------
 - Description: Parses a given string representation of a Nat and
   returns a member of Nat.
 -}
natParse :: String -> Nat
natParse n = parse (drop 4 n)
    where
        parse :: String -> Nat
        parse ('S':xs) = S (parse xs)
        parse _ = Z

instance Read Nat where         -- Implements Nat as part of the Read Class using the NatParse function
    readsPrec _ n = [(natParse n, "")]

{- -----------------------------------------------------------------
 - binNatParse
 - -----------------------------------------------------------------
 - Description: Parses a given string representation of a BinNat and
   returns a member of BinNat.
 -}
binNatParse :: String -> BinNat
binNatParse n = parse (drop 7 n)
    where
        -- Uses ViewPatterns to pattern match to the last character in the string representation
        parse :: String -> BinNat
        parse ['0'] = Atom Zero
        parse ['1'] = Atom One
        parse (reverse -> '1':(reverse -> xs)) = Compound (parse xs) One
        parse (reverse -> '0':(reverse -> xs)) = Compound (parse xs) Zero

instance Read BinNat where       -- Implements BinNat as part of the Read Class using the binNatParse function
    readsPrec _ n = [(binNatParse n, "")]

{- -----------------------------------------------------------------
 - plus
 - -----------------------------------------------------------------
 - Description: Returns a Nat that results from the addition of 
   two given Nats.
 -}
plus :: Nat -> Nat -> Nat
plus Z n = n
plus (S n) m = S (plus n m)

{- -----------------------------------------------------------------
 - time
 - -----------------------------------------------------------------
 - Description: Returns a Nat that results from the multiplication of 
   two given Nats.
 -}
time :: Nat -> Nat -> Nat
time Z _ = Z
time (S n) m = plus m (time n m)

{- -----------------------------------------------------------------
 - binPlus
 - -----------------------------------------------------------------
 - Description: Returns a BinNat that results from the addition of 
   two given BinNats.
 -}
binPlus :: BinNat -> BinNat -> BinNat
binPlus (Atom One) (Atom One) = Compound (Atom One) Zero
binPlus (Atom n) (Atom m) = Atom (n `add` m)
binPlus (Atom n) (Compound a d) = Compound (binPlus a (Atom (n `carry` d))) (n `add` d)
binPlus (Compound a d) (Atom n) = Compound (binPlus a (Atom (n `carry` d))) (n `add` d)
binPlus (Compound a b) (Compound c d) = Compound (binPlus (binPlus a (Atom (b `carry` d))) c) (b `add` d)

-- Helper function for adding binNats
-- Returns the digit resulting from an addition in binary.
add :: Digit -> Digit -> Digit
add Zero n = n
add n Zero = n
add One One = Zero

-- Helper function for adding binNats
-- Returns the "carried" digit resulting from an addition in binary
carry :: Digit -> Digit -> Digit
carry Zero _ = Zero
carry _ Zero = Zero
carry One One = One

{- -----------------------------------------------------------------
 - binTimes
 - -----------------------------------------------------------------
 - Description: Returns a BinNat that results from the multiplication of 
   two given BinNats.
 -}
binTimes :: BinNat -> BinNat -> BinNat
binTimes (Atom One) n = n
binTimes (Atom Zero) _ = Atom Zero
binTimes (Compound a One) n = binPlus n $ binTimes a $ Compound n Zero
binTimes (Compound a Zero) n = binTimes a $ Compound n Zero



