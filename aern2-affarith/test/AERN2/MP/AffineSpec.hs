-- |
--    Module      :  AERN2.MP.BallSpec
--    Description :  hspec tests for MPBall
--    Copyright   :  (c) Michal Konecny
--    License     :  BSD3
--
--    Maintainer  :  mikkonecny@gmail.com
--    Stability   :  experimental
--    Portability :  portable
module AERN2.MP.AffineSpec (spec) where

import AERN2.MP (MPBall, CanSetPrecision (setPrecision), IsInterval (endpoints), mpBall, mpBallP, prec)
import AERN2.MP.Affine
import AERN2.MP.Affine.Tests (specMPAffine)
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Float (mpFloat)
import Data.Map qualified as Map
import MixedTypesNumPrelude
import Test.Hspec

spec :: Spec
spec = do
  specMPAffine
  describe "trigonometric correlation" $ do
    let wideX = variable (int 1) 4 (rational 4) -- [0,8]
        wideY = variable (int 2) 4 (rational 4)
    it "keeps independent sine inputs with the same fallback range independent" $
      mpBall (sin wideX - sin wideY) `shouldEnclose` (mpBall (-2), mpBall 2)
    it "keeps independent cosine inputs with the same fallback range independent" $
      mpBall (cos wideX - cos wideY) `shouldEnclose` (mpBall (-2), mpBall 2)
    it "distinguishes sine and cosine with the same fallback range" $
      mpBall (sin wideX - cos wideX) `shouldEnclose` (mpBall (-1), mpBall 1)
    mapM_ checkCorrelation
      [ ("sine fallback", sin, sin, 4, rational 4),
        ("cosine fallback", cos, cos, 4, rational 4),
        ("increasing sine", sin, sin, 0, 1 / 8),
        ("decreasing sine", sin, sin, 3, 1 / 8),
        ("increasing cosine", cos, cos, -2, 1 / 8),
        ("decreasing cosine", cos, cos, 2, 1 / 8)
      ]

variable :: Int -> Integer -> Rational -> MPAffine
variable termId c radius =
  MPAffine
    { config = MPAffineConfig {maxTerms = int 20, precision = 100},
      centre = setPrecision (prec 100) (mpFloat (dyadic c)),
      errTerms = Map.singleton (ErrorTermId termId) (setPrecision (prec 100) (mpFloat (dyadic radius)))
    }

checkCorrelation :: (String, MPAffine -> MPAffine, MPBall -> MPBall, Integer, Rational) -> Spec
checkCorrelation (label, fAffine, fBall, c, radius) =
  describe label $ do
    let x = variable (int 1) c radius
        y = variable (int 2) c radius
        lo = fBall (mpBallP (prec 100) (c - radius))
        hi = fBall (mpBallP (prec 100) (c + radius))
    it "retains cancellation for the same expression" $
      mpBall (fAffine x - fAffine x) `shouldSatisfy` isCertainlyZero
    it "encloses endpoint differences for independent inputs" $
      mpBall (fAffine x - fAffine y) `shouldEnclose` (lo - hi, hi - lo)

shouldEnclose :: MPBall -> (MPBall, MPBall) -> Expectation
shouldEnclose enclosure (x, y) = do
  let (lower, upper) = endpoints enclosure
      (xLower, xUpper) = endpoints x
      (yLower, yUpper) = endpoints y
  lower `shouldSatisfy` (<= min xLower yLower)
  upper `shouldSatisfy` (>= max xUpper yUpper)
