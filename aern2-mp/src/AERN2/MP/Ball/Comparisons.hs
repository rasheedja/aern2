{-|
    Module      :  AERN2.MP.Ball.Comparisons
    Description :  Comparisons of arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Comparisons of arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Comparisons
(
  -- * Auxiliary types
  module AERN2.Norm
  -- * Ball operations (see also instances)
  , reducePrecionIfInaccurate
  -- * Helpers for constructing ball functions
  , byEndpointsMP
  -- * intersection and hull
  , intersectCNMPBall
  , hullMPBall
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.CollectErrors

import AERN2.Norm
import AERN2.MP.Dyadic (Dyadic)
import AERN2.MP.Float (MPFloat)
-- import AERN2.MP.Float.Operators
import AERN2.MP.Precision

import AERN2.MP.Ball.Type
import AERN2.MP.Ball.Conversions ()

{- comparisons -}

instance HasEqAsymmetric MPBall MPBall where
  type EqCompareType MPBall MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Integer where
  type EqCompareType MPBall Integer = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Integer MPBall where
  type EqCompareType Integer MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Int where
  type EqCompareType MPBall Int = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Int MPBall where
  type EqCompareType Int MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Rational where
  type EqCompareType MPBall Rational = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Rational MPBall where
  type EqCompareType Rational MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance HasEqAsymmetric MPBall Dyadic where
  type EqCompareType MPBall Dyadic = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2
instance HasEqAsymmetric Dyadic MPBall where
  type EqCompareType Dyadic MPBall = Maybe Bool
  b1 `equalTo` b2 =   b1 >= b2 && b1 <= b2

instance
  (HasEqAsymmetric MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (EqCompareType MPBall b)
  , IsBool (EnsureCE es (EqCompareType MPBall b))
  , SuitableForCE es)
  =>
  HasEqAsymmetric MPBall (CollectErrors es  b)
  where
  type EqCompareType MPBall (CollectErrors es  b) =
    EnsureCE es (EqCompareType MPBall b)
  equalTo = lift2TLCE equalTo

instance
  (HasEqAsymmetric a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (EqCompareType a MPBall)
  , IsBool (EnsureCE es (EqCompareType a MPBall))
  , SuitableForCE es)
  =>
  HasEqAsymmetric (CollectErrors es a) MPBall
  where
  type EqCompareType (CollectErrors es  a) MPBall =
    EnsureCE es (EqCompareType a MPBall)
  equalTo = lift2TCE equalTo

instance HasOrderAsymmetric MPBall MPBall where
  type OrderCompareType MPBall MPBall = Maybe Bool
  lessThan b1 b2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpoints b1
    (l2, r2) = endpoints b2
  leq b1 b2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpoints b1
    (l2, r2) = endpoints b2

instance HasOrderAsymmetric Integer MPBall where
  type OrderCompareType Integer MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Integer where
  type OrderCompareType MPBall Integer = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Int MPBall where
  type OrderCompareType Int MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Int where
  type OrderCompareType MPBall Int = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric Dyadic MPBall where
  type OrderCompareType Dyadic MPBall = Maybe Bool
  lessThan = convertFirst lessThan
  leq = convertFirst leq
instance HasOrderAsymmetric MPBall Dyadic where
  type OrderCompareType MPBall Dyadic = Maybe Bool
  lessThan = convertSecond lessThan
  leq = convertSecond leq

instance HasOrderAsymmetric MPBall Rational where
  type OrderCompareType MPBall Rational = Maybe Bool
  lessThan b1 q2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpoints b1
    l2 = q2
    r2 = q2
  leq b1 q2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l1, r1) = endpoints b1
    l2 = q2
    r2 = q2

instance HasOrderAsymmetric Rational MPBall where
  type OrderCompareType Rational MPBall = Maybe Bool
  lessThan q1 b2
    | r1 < l2 = Just True
    | r2 <= l1 = Just False
    | otherwise = Nothing
    where
    (l2, r2) = endpoints b2
    l1 = q1
    r1 = q1
  leq q1 b2
    | r1 <= l2 = Just True
    | r2 < l1 = Just False
    | otherwise = Nothing
    where
    (l2, r2) = endpoints b2
    l1 = q1
    r1 = q1

instance
  (HasOrderAsymmetric MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (OrderCompareType MPBall b)
  , IsBool (EnsureCE es (OrderCompareType MPBall b))
  , SuitableForCE es)
  =>
  HasOrderAsymmetric MPBall (CollectErrors es  b)
  where
  type OrderCompareType MPBall (CollectErrors es  b) =
    EnsureCE es (OrderCompareType MPBall b)
  lessThan = lift2TLCE lessThan
  leq = lift2TLCE leq
  greaterThan = lift2TLCE greaterThan
  geq = lift2TLCE geq

instance
  (HasOrderAsymmetric a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (OrderCompareType a MPBall)
  , IsBool (EnsureCE es (OrderCompareType a MPBall))
  , SuitableForCE es)
  =>
  HasOrderAsymmetric (CollectErrors es a) MPBall
  where
  type OrderCompareType (CollectErrors es  a) MPBall =
    EnsureCE es (OrderCompareType a MPBall)
  lessThan = lift2TCE lessThan
  leq = lift2TCE leq
  greaterThan = lift2TCE greaterThan
  geq = lift2TCE geq

instance CanTestZero MPBall
instance CanTestPosNeg MPBall

instance CanTestInteger MPBall where
  certainlyNotInteger b =
    (rN - lN) == 1 && lN !<! b && b !<! rN
    where
      (lN, rN) = integerBounds b
  certainlyIntegerGetIt b
    | rN == lN = Just lN
    | otherwise = Nothing
    where
      (lN, rN) = integerBounds b

instance CanMinMaxAsymmetric MPBall MPBall where
  min = byEndpointsMP min
  max = byEndpointsMP max

instance CanMinMaxAsymmetric MPBall Integer where
  type MinMaxType MPBall Integer = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Integer MPBall where
  type MinMaxType Integer MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Int where
  type MinMaxType MPBall Int = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Int MPBall where
  type MinMaxType Int MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Dyadic where
  type MinMaxType MPBall Dyadic = MPBall
  min = convertSecond min
  max = convertSecond max
instance CanMinMaxAsymmetric Dyadic MPBall where
  type MinMaxType Dyadic MPBall = MPBall
  min = convertFirst min
  max = convertFirst max

instance CanMinMaxAsymmetric MPBall Rational where
  type MinMaxType MPBall Rational = MPBall
  min = convertPSecond min
  max = convertPSecond max
instance CanMinMaxAsymmetric Rational MPBall where
  type MinMaxType Rational MPBall = MPBall
  min = convertPFirst min
  max = convertPFirst max

instance
  (CanMinMaxAsymmetric MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (MinMaxType MPBall b)
  , SuitableForCE es)
  =>
  CanMinMaxAsymmetric MPBall (CollectErrors es  b)
  where
  type MinMaxType MPBall (CollectErrors es  b) =
    EnsureCE es (MinMaxType MPBall b)
  min = lift2TLCE min
  max = lift2TLCE max

instance
  (CanMinMaxAsymmetric a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (MinMaxType a MPBall)
  , SuitableForCE es)
  =>
  CanMinMaxAsymmetric (CollectErrors es a) MPBall
  where
  type MinMaxType (CollectErrors es  a) MPBall =
    EnsureCE es (MinMaxType a MPBall)
  min = lift2TCE min
  max = lift2TCE max

{- intersection -}

instance CanIntersectAsymmetric MPBall MPBall where
  intersect a b
    | l > r =
        noValueNumErrorCertainCN $ NumError $ "intersect: empty intersection: " ++ show a ++ "; " ++ show b
    | otherwise = cn $ setPrecision p $ fromMPFloatEndpoints l r
    where
    p  = getPrecision a
    l = max aL bL
    r = min aR bR
    (aL,aR) = endpoints a
    (bL,bR) = endpoints b

intersectCNMPBall :: CN MPBall -> CN MPBall -> CN MPBall
intersectCNMPBall x y =
  case (fst $ ensureNoCN x, fst $ ensureNoCN y) of 
    (Nothing, Nothing) -> x
    (Just _ , Nothing) -> x
    (Nothing, Just _ ) -> y
    (Just _ , Just _ ) -> lift2CE intersect x y

instance
  (CanIntersectAsymmetric MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (IntersectionType MPBall b)
  , SuitableForCE es)
  =>
  CanIntersectAsymmetric MPBall (CollectErrors es b)
  where
  type IntersectionType MPBall (CollectErrors es b) =
    EnsureCE es (IntersectionType MPBall b)
  intersect = lift2TLCE intersect

instance
  (CanIntersectAsymmetric a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (IntersectionType a MPBall)
  , SuitableForCE es)
  =>
  CanIntersectAsymmetric (CollectErrors es a) MPBall
  where
  type IntersectionType (CollectErrors es  a) MPBall =
    EnsureCE es (IntersectionType a MPBall)
  intersect = lift2TCE intersect

{- union -}

hullMPBall :: MPBall -> MPBall -> MPBall
hullMPBall a b = 
  fromEndpoints rL rR
  where
  rL = min aL bL
  rR = max aR bR
  (aL,aR) = endpoints a
  (bL,bR) = endpoints b


instance CanUnionAsymmetric MPBall MPBall where
  union a b =
    case getMaybeValueCN (a `intersect` b) of
      Just _ -> prependErrorsCN [(ErrorPotential, err)] r
      _ -> prependErrorsCN [(ErrorCertain, err)] r
    where
    err = NumError $ "union of enclosures: not enclosing the same value"
    r = cn $ hullMPBall a b


instance
  (CanUnionAsymmetric MPBall b
  , CanEnsureCE es b
  , CanEnsureCE es (UnionType MPBall b)
  , SuitableForCE es)
  =>
  CanUnionAsymmetric MPBall (CollectErrors es b)
  where
  type UnionType MPBall (CollectErrors es b) =
    EnsureCE es (UnionType MPBall b)
  union = lift2TLCE union

instance
  (CanUnionAsymmetric a MPBall
  , CanEnsureCE es a
  , CanEnsureCE es (UnionType a MPBall)
  , SuitableForCE es)
  =>
  CanUnionAsymmetric (CollectErrors es a) MPBall
  where
  type UnionType (CollectErrors es  a) MPBall =
    EnsureCE es (UnionType a MPBall)
  union = lift2TCE union

{-|
  Compute an MPBall function from *exact* MPFloat operations on interval endpoints.
  This works only for *non-decreasing* operations, eg addition, min, max.
-}
byEndpointsMP ::
    (MPFloat -> MPFloat -> MPFloat) ->
    (MPBall -> MPBall -> MPBall)
byEndpointsMP op b1 b2 =
    fromEndpoints (l1 `op` l2) (r1 `op` r2)
    where
    (l1,r1) = endpoints b1
    (l2,r2) = endpoints b2

{-  random generation -}
