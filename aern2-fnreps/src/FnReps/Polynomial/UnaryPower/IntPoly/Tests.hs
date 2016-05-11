{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FnReps.Polynomial.UnaryPower.IntPoly.Tests where

import AERN2.Num -- alternative Prelude
import qualified Prelude as P

import qualified Data.List as List
import Data.Ratio

import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

import FnReps.Polynomial.UnaryPower.IntPoly.Basics
import FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding

data IntPolyWithRoots =
    IntPolyWithRoots
    {
        intPolyWithRoots_poly :: IntPoly,
        intPolyWithRoots_denominator :: Integer,
        intPolyWithRoots_rootsSorted :: [(Rational, RootMultiplicity)]
    }
    deriving (Show)

testIsolateRootsRepeatable :: Integer -> Bool -> IO ()
testIsolateRootsRepeatable seedI isVerbose 
    | isVerbose =
        verboseCheckWith args isolateRootsIsCorrect
    | otherwise =
        quickCheckWith args isolateRootsIsCorrect
    where
    seed = int seedI
    args = stdArgs { replay = Just (mkQCGen seed, seed) }

isolateRootsIsCorrect :: IntPolyWithRoots -> Property
isolateRootsIsCorrect (IntPolyWithRoots intpoly _denom rootsMSorted) =
    allRootsContainedInResult 
    .&&. 
    eachIntervalContainsRoot
    where
    allRootsContainedInResult =
        and [ inResult root | root <- rootsSorted ]
    inResult root =
        or [ root `containedIn` interval | interval <- isolateRootsResult ]
    
    eachIntervalContainsRoot =
        and [ hasRoot interval | interval <- isolateRootsResult ]
    hasRoot interval =
        or [ root `containedIn` interval | root <- rootsSorted ]
    
    a `containedIn` (Interval l r) = l <= a && a <= r
          
    isolateRootsResult = isolateRoots l r intpoly
        where
        l = -1 + (minimum rootsSorted)
        r = 1 + (maximum rootsSorted)
     
    rootsSorted = map fst rootsMSorted

{-
    Selection of real polynomials + their roots.
    
    First, randomly select: 
        * a small number of rational monomials (roots)
        * a small number of rational binomials (complex root pairs) 
        * the multiplicity of these roots/root pairs

    Then multiply these monomials and binomials,
    then convert integer polynomial + denominator of the form 2^n.
-}

instance Arbitrary IntPolyWithRoots where
    arbitrary =
        do
        -- a number of rational roots:
        rootsPre <- sizedListOf 1 0.25 arbitraryRational
        let roots = List.nub $ List.sort rootsPre -- remove duplicate roots
        -- multiplicities for the roots:
        multiplicities <- vectorOf (length roots) arbitrary 
        -- TODO: generate binomials with no real roots
        return $ roots2poly $ zip roots multiplicities
        where
        sizedListOf offset scaling gen = 
            sized $ \s -> resize (P.round $ offset+(integer s)*scaling) $ listOf (resize s gen)
        roots2poly roots =
            IntPolyWithRoots poly denom roots
            where
            poly = List.foldl' (*) (fromList [(0,1)]) monomials
            monomials = concat $ map monoms roots
                where
                monoms (root, RootMultiplicity i) = 
                    replicate (int i) $ 
                        fromList [(1,denom), (0, numerator $ -root*denom)] -- (x - root)^i
            denominators = map (denominator . fst) roots
            denom = foldl lcm 1 denominators 
            

arbitraryRational :: Gen Rational
arbitraryRational =
    do
    a <- scale (int . (\n -> n*n) . integer) arbitrary
    Positive b <- arbitrary
    let _ = [a,b] :: [Integer]
    return $ a/b

newtype RootMultiplicity = RootMultiplicity Integer
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance Arbitrary RootMultiplicity
    where
    arbitrary = 
        frequency $ 
            [(int $ (4-i)^3, elements [RootMultiplicity i]) | i <- [1..4]]
            -- 1 is more likely than 2 etc.


{- TODO The following section in not needed in this file. It should be moved. -}  

{- Selection of real numbers. 
    Integers and rational number should be generated with a significant probability.
    -1,0,1,pi,2pi,pi/2 should be also generated with a significant probability.
    Sometimes randomly generate an integer part + signed-binary sequence.
-}

newtype CauchyRealForArbitrary = CauchyRealForArbitrary CauchyReal

data SignedBinaryDigit = SBPos | SBZer | SBNeg
data SignedBinaryReal =
    SignedBinaryReal
    {
        sbReal_integerPart :: Integer,
        sbReal_digits :: [SignedBinaryDigit]
    }
    
    