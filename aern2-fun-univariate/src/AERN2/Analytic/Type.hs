module AERN2.Analytic.Type
(
    Analytic(..)
  , UnitInterval (..)
  , toUnitInterval
  , fromUnitInterval
  , lift1UI
  , ana_derivative
)
where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.RealFun.Operations
import AERN2.Real


data Analytic =
  Analytic Integer Integer (Integer -> CauchyReal) -- TODO: replace integer with something more fine-grained
  --- (A, k, (a_n)_n)
  --- |a_n| \leq A * (2^{1/k})^{-n}

data UnitInterval =
  UI CauchyReal

toUnitInterval :: CauchyReal -> UnitInterval
toUnitInterval x = UI (max (-1) (min x 1))

fromUnitInterval :: UnitInterval -> CauchyReal
fromUnitInterval (UI x) = x

lift1UI :: (CauchyReal -> a) -> UnitInterval -> a
lift1UI f (UI x) = f x

instance CanApply Analytic CauchyReal where
  type ApplyType Analytic CauchyReal = CauchyReal
  apply f x = apply f (toUnitInterval x)

instance CanApply Analytic UnitInterval where
  type ApplyType Analytic UnitInterval = CauchyReal
  apply _f@(Analytic a k a_n) x =
    realLim
      (\m ->  horn m (a_n m)) -- TODO: evaluate more cleverly
      (\m -> a * q^!(m + 1) /! (1 - q))
    where
    q = abs (fromUnitInterval x) /! (2^!(1/!k))
    horn 0 y = y
    horn i y = horn (i - 1) (y * (fromUnitInterval x) + (a_n (i - 1)))

ana_derivative :: Analytic -> Analytic
ana_derivative _f@(Analytic a k a_n) =
  Analytic da dk da_n
  where
  da   = a*(1 + dk) -- TODO: implement something tighter
  dk   = 2*k
  da_n n = (n + 1) * (a_n (n + 1))

realLim :: (Integer -> CauchyReal) -> (Integer -> CauchyReal) -> CauchyReal
realLim xe_n err_n =
  newCR "lim" [] makeQ
  where
  makeQ _me_src acc@(AccuracySG s _) = h 0
    where
    h k
      | kthOk =
        centreAsBall ((xe_n k) ? (acc + 1)) -- TODO: is this correct?
        + (mpBall (0, (dyadic 0.5)^!(fromAccuracy s)))
      | otherwise =
        h (k + 1)
      where
      kthError :: MPBall
      kthError = (err_n k) ? (acc + 2)
      kthOk :: Bool
      kthOk =
        (kthError <= 0.5^!((fromAccuracy s) + 1)) == Just True
