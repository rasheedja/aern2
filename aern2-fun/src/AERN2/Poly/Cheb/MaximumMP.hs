{-# LANGUAGE CPP #-}
#define DEBUG
module AERN2.Poly.Cheb.MaximumMP
(
maximum,
maximumOptimised,
maximumOptimisedWithAccuracy,
minimum,
minimumOptimised,
minimumOptimisedWithAccuracy
) where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude hiding (maximum, minimum)

--import Text.Printf

import AERN2.MP.Ball
import AERN2.MP.Dyadic
import qualified Data.Map as Map

-- import AERN2.Poly.Basics (terms_updateConst)

import qualified AERN2.Poly.Power as Pow

import AERN2.RealFun.Operations

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Eval
import AERN2.Poly.Cheb.Derivative
import AERN2.Poly.Conversion
import AERN2.Interval

chPolyBoundsError :: ChPolyBounds c
chPolyBoundsError = error "ChPolyBounds undefined in internal MaximumMP functions"

maximum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
maximum (ChPoly dom poly acG _) l r  =
   Pow.genericMaximum (evalDf f df)
    (Map.fromList [(0, (evalDirect dfc, cheb2Power $ chPoly_poly dfc))])
    (getAccuracy f)
    (fromDomToUnitInterval dom l) (fromDomToUnitInterval dom r)
   where
   f  = makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly acG chPolyBoundsError
   df = makeExactCentre $ derivative f
   dfc = derivative $ centre f

maximumOptimisedWithAccuracy
  :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimisedWithAccuracy acc (ChPoly dom poly acG _) l r initialDegree steps =
    {-trace("maximum optimised... ")$
    trace("f: "++(show f))$
    trace("df: "++(show fc'))$
    trace("dfs: "++(show dfs))$-}
    Pow.genericMaximum
      (evalDf f (reduceToEvalDirectAccuracy fc' (bits $ -4))) dfsWithEval
      (min (getAccuracy f) acc)
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) l))
      (fromDomToUnitInterval dom (setPrecision (getPrecision f) r))
  where
  reduceDegreeToAccuracy d g =
    let
      try = reduceDegree d g
    in
      if getAccuracy try >= acc then
        try
      else
        reduceDegreeToAccuracy (d + 5) g
  f   = reduceDegreeToAccuracy 5 $ makeExactCentre $ ChPoly (dyadicInterval (-1,1)) poly acG chPolyBoundsError
  fc' = (makeExactCentre . derivativeExact . centre) f
  maxKey = max 0 (ceiling ((degree f - initialDegree) /! steps))
  ch2Power :: ChPoly MPBall -> Pow.PowPoly MPBall
  ch2Power p =
    let
      err = mpBall $ dyadic $ radius p
    in
    (fromEndpointsAsIntervals (-err) err) + (cheb2Power . chPoly_poly . centre) p
  dfsWithEval =
    Map.fromList
    [(k,(evalDirect df :: MPBall -> MPBall, ch2Power df)) | (k,df) <- dfs]
  dfs = [(k, reduceDegree (initialDegree + steps*k) fc') | k <- [0..maxKey + 1]]

maximumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
maximumOptimised f =
  maximumOptimisedWithAccuracy (getFiniteAccuracy f) f

minimum :: ChPoly MPBall -> MPBall -> MPBall -> MPBall
minimum f l r = -(maximum (-f) l r)

minimumOptimisedWithAccuracy :: Accuracy -> ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimisedWithAccuracy acc f l r iDeg steps = -(maximumOptimisedWithAccuracy acc (-f) l r iDeg steps)

minimumOptimised :: ChPoly MPBall -> MPBall -> MPBall -> Integer -> Integer -> MPBall
minimumOptimised f = minimumOptimisedWithAccuracy (getFiniteAccuracy f) f

instance CanMinimiseOverDom (ChPoly MPBall) DyadicInterval where
  type MinimumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  minimumOverDom f (Interval l r) =
    minimumOptimised (setPrecision (3*getPrecision f) f) (mpBall l) (mpBall r) 5 5
    {-res
    where
    (_, Just res) = last $ iterateUntilAccurate ac withPrec
    ac = getFiniteAccuracy f
    withPrec p =
      maybeTrace (printf "ChPoly: MinimumOverDomType: withPrec: p = %s; ac = %s"
        (show p) (show $ getAccuracy resP)) $
      Just resP
      where
      resP = minimumOptimised (setPrecision p f) (mpBall l) (mpBall r) 5 5-}

instance CanMaximiseOverDom (ChPoly MPBall) DyadicInterval where
  type MaximumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  maximumOverDom f (Interval l r) =
    maximumOptimised (setPrecision (3*getPrecision f) f) (mpBall l) (mpBall r) 5 5
