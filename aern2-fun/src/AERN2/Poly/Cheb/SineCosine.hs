{-|
    Module      :  AERN2.Poly.Cheb.SineCosine
    Description :  Sine and cosine for polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Sine and cosine for polynomials
-}

module AERN2.Poly.Cheb.SineCosine
-- (
-- )
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import qualified Data.Map as Map
import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.MP.ErrorBound
import AERN2.MP.Float
import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()
import AERN2.Poly.Cheb.Eval

import Debug.Trace (trace)


shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace
    | shouldTrace = trace
    | otherwise = const id


_test10Xe :: ChPoly MPBall
_test10Xe =
    10*x
    where
    x :: ChPoly MPBall
    x = setPrecision (prec 100) $ varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

_testSine10X :: ChPoly MPBall
_testSine10X =
    sineWithPrecDegSweep (prec 100) 100 NormZero (10*x)
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

_testSine10Xe :: ChPoly MPBall
_testSine10Xe =
    sineWithPrecDegSweep (prec 100) 100 NormZero (updateRadius (+ (errorBound 0.1)) (10*x))
    where
    x :: ChPoly MPBall
    x = varFn sampleFn ()
    sampleFn = constFn (dom, 1)
    dom = dyadicInterval (0.0,1.0)

{-
    To compute sin(xC+-xE):

    * compute (rC+-rE) = range(xC)
    * compute k = round(rC/(pi/2))
    * compute sin or cos of txC = xC-k*pi/2 using Taylor series
      * use sin for even k and cos for odd k
      * which degree to use?
        * keep trying higher and higher degrees until
            * the accuracy of the result worsens
            * OR the accuracy of the result is 8x higher than xE
    * if k mod 4 = 2 then negate result
    * if k mod 4 = 3 then negate result
    * add xE to the error bound of the resulting polynomial
-}

sineWithPrecDegSweep ::
  -- (Field c, CanMinMaxSameType c,
  --  CanAbsSameType c,
  --  CanAddSubMulDivBy c CauchyReal,
  --  ConvertibleExactly Dyadic c,
  --  HasNorm c,  CanRound c,
  --  IsBall c, IsInterval c c,
  --  CanApply (ChPoly c) c, ApplyType (ChPoly c) c ~ c)
  -- =>
  Precision -> Degree -> NormLog -> ChPoly MPBall -> ChPoly MPBall
sineWithPrecDegSweep prc maxDeg sweepT xPre =
    maybeTrace
    (
        "ChPoly.sineWithDegSweep:"
        ++ "\n maxDeg = " ++ show maxDeg
        -- ++ "\n xC = " ++ showAP xC
        -- ++ "\n xE = " ++ showB xE
        ++ "\n xAccuracy = " ++ show xAccuracy
        ++ "\n r = " ++ show r
        -- ++ "\n r = " ++ showB r
        ++ "\n k = " ++ show k
        -- ++ "\n txC = " ++ showAP txC
        -- ++ "\n trM = " ++ showB trM
        -- ++ "\n taylorSumE = " ++ showB taylorSumE
        -- ++ "\n resC = " ++ showAP resC
    ) $
--    xPoly (prec 100) -- dummy
    res
    where
    -- showB = show . getApproximate (bits 30)
    -- showAP = show . getApproximate (bits 50) . cheb2Power

    -- first separate the centre of the polynomial x from its radius:
    x = setPrecision prc xPre
    xC = centre x
    xE = radius x
    xAccuracy = getAccuracy x

    -- compute (rC+-rE) = range(xC):
    Interval rL rR =
      sampledRange (dyadicInterval (-1.0,1.0)) 5 xC
    r = fromEndpoints rL rR
    _ = [r,rL,rR] :: [MPBall]
    rC = centreAsBall r

    -- compute k = round(rC/(pi/2)):
    k = floor $ (fst $ endpoints $ 0.5 + (2*rC / pi) :: MPFloat)

    -- shift xC near 0 using multiples of pi/2:
    txC = xC - k * pi / 2
    -- work out an absolute range bound for txC:
    (_, trM) = endpoints $ abs $ r - k * pi / 2
    _ = [trM, r]

    -- compute sin or cos of txC = xC-k*pi/2 using Taylor series:
    taylorSums
        | even k = sineTaylorSeries maxDeg sweepT txC
        | otherwise = cosineTaylorSeries maxDeg sweepT txC
    (taylorSum, taylorSumE) = pickByAccuracy [] taylorSums
        where
        pickByAccuracy prevResults (_s@(p, e, n) : rest) =
            maybeTrace
            ("pickByAccuracy: sE = " ++ show sE ++ "; sAccuracy = " ++ show sAccuracy ++ "; prec = " ++ show (getPrecision pBest)) $
            pbAres
            where
            pbAres
              | tooAccurate || stoppedMakingProgress || sameAccuracyCount > 10 =
                (centre pBest, sEBest)
              | otherwise =
                pickByAccuracy ((sAccuracy, p, sE) : prevResults) rest
            tooAccurate = sAccuracy >= xAccuracy + 3
            prevAccuracies = map (\(a,_,_) -> a) prevResults
            sameAccuracyCount =
                case List.findIndex (/= sAccuracy) prevAccuracies of Just i -> integer i; _ -> 0
            (stoppedMakingProgress, pBest, sEBest) =
                case prevResults of
                    ((a1,p1,sE1):(a2,_,_):(a3,_,_):(a4,_,_):_)
                        | sAccuracy < a1 && a1 > a2 -> (True, p1, sE1)
                        | sAccuracy == a1 && a1 == a2 && a2 == a3 && a3 == a4 -> (True, p, sE)
                    _ -> (False, p, sE)
            sE = (errorBound $ e*(trM^n)) + (radius p)
            sAccuracy = normLog2Accuracy $ getNormLog $ dyadic sE
        pickByAccuracy _ _ = error "internal error in SineCosine"
    -- if k mod 4 = 2 then negate result,
    -- if k mod 4 = 3 then negate result:
    resC
        | k `mod` 4 == 2 = -taylorSum
        | k `mod` 4 == 3 = -taylorSum
        | otherwise = taylorSum
    -- add xE to the error bound of the resulting polynomial:
    res = updateRadius (+ (taylorSumE + xE)) resC

{-|
    For a given polynomial @p@, compute all partial Taylor sums of @sin(p)@ and return
    them together with @e@, an error bound on @[-1,1]@, and a number @n@.
    The number @n@ can be used to obtain a better error bound on a domain @[-a,a]@
    for some @0 <= a < 1@.  The better error bound is @e*a^n@.
-}
sineTaylorSeries ::
  (Ring c, CanDivBy c Integer, IsInterval c c, HasNorm c)
  =>
  Degree -> NormLog -> ChPoly c -> [(ChPoly c, Rational, Integer)]
sineTaylorSeries maxDeg sweepT x =
    let
    termComponents =
        iterate addNextTerm (0,1,1,6,Map.singleton 1 x)
        where
        addNextTerm (prevI, prevN, _prevFact, currentFact, prevPowers) =
            (i, n, currentFact, nextFact, newPowers)
            where
            i = prevI + 1
            n = prevN + 2
            nextFact = currentFact*((n+1)*(n+2))
            newPowers = Map.insert n (reduce currentPower) prevPowers
            reduce = reduceDegreeAndSweep maxDeg sweepT
            currentPower
                | odd i = x * (power i) * (power i)
                | otherwise = x * (power (i-1)) * (power (i+1))
                where
                power j = lookupForce j prevPowers
    sumsAndErrors =
        makeSums (chPoly (x,0), 1) termComponents
        where
        makeSums (prevSum, sign) ((_i, n, nFact, nextFact, xPowers) : rest) =
            (newSum, 1/nextFact, n+2) : makeSums (newSum, -sign) rest
            where
            newSum = prevSum + sign*xPowN/nFact
            xPowN = lookupForce n xPowers
        makeSums _ _ = error "internal error in SineCosine.sineTaylorSeries"
    in
    sumsAndErrors

{-|
    For a given polynomial @p@, compute all partial Taylor sums of @cos(p)@ and return
    them together with @e@, an error bound on @p\in[-1,1]@, and a number @n@.
    The number @n@ can be used to obtain an error bound for @p\in[-a,a]@
    for some @0 <= a@.  The error bound is @e*a^n@.
-}
cosineTaylorSeries ::
  (Ring c, CanDivBy c Integer, IsInterval c c, HasNorm c)
  =>
  Degree -> NormLog -> ChPoly c -> [(ChPoly c, Rational, Integer)]
cosineTaylorSeries maxDeg sweepT x =
    let
    termComponents =
        iterate addNextTerm (1,2,2,24,Map.singleton 2 (x*x))
        where
        addNextTerm (prevI, prevN, _prevFact, currentFact, prevPowers) =
            (i, n, currentFact, nextFact, newPowers)
            where
            i = prevI + 1
            n = prevN + 2
            nextFact = currentFact*((n+1)*(n+2))
            newPowers = Map.insert n (reduce currentPower) prevPowers
            reduce = reduceDegreeAndSweep maxDeg sweepT
            currentPower
                | even i = (power i) * (power i)
                | otherwise = (power (i-1)) * (power (i+1))
                where
                power j = lookupForce j prevPowers
    sumsAndErrors =
        makeSums (chPoly (x, 1), -1) termComponents
        where
        makeSums (prevSum, sign) ((_i, n, nFact, nextFact, xPowers) : rest) =
            (newSum, 1/nextFact, n+2) : makeSums (newSum, -sign) rest
            where
            newSum = prevSum + sign*xPowN/nFact
            xPowN = lookupForce n xPowers
        makeSums _ _ = error "internal error in SineCosine.cosineTaylorSeries"
    in
    sumsAndErrors

lookupForce :: P.Ord k => k -> Map.Map k a -> a
lookupForce j amap =
    case Map.lookup j amap of
        Just t -> t
        Nothing -> error "internal error in SineCosine.lookupForce"
