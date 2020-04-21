module AERN2.BoxFun.TestFunctions where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import qualified Data.List as List

import AERN2.AD.Differential hiding (x)
-- import AERN2.Util.Util
import AERN2.BoxFun.Type
import AERN2.Linear.Vector.Type ((!), Vector)
import qualified AERN2.Linear.Vector.Type as V


symmetricDomain :: Integer -> Rational -> Rational -> Vector (CN MPBall)
symmetricDomain n l r = 
    V.map (\_ -> fromEndpointsAsIntervals (cn $ mpBallP (prec 53) $ l) (cn $  mpBallP (prec 53) r)) $ V.enumFromTo 1 n

griewank :: Integer -> BoxFun
griewank n =
    BoxFun
    n
    (\v ->
        let
            ord = order (v ! 0)
            sm  = List.foldl' (+) (differential ord (cn $ mpBall 0)) [let x = (v ! k) in x^2 | k <- [0 .. V.length v - 1]]
            prd = List.foldl' (*) (differential ord (cn $ mpBall 1)) [cos $ (v ! k) / (sqrt $ 1 + mpBallP p k) | k <- [0 .. V.length v - 1]]
            p   = getPrecision v
        in
        1 + ((setPrecision p $ mpBall 1)/(setPrecision p $ mpBall 4000)) * sm - prd
    )
    (symmetricDomain n (-600.0) 600.0)

rosenbrock :: BoxFun
rosenbrock =
    BoxFun
    2
    (\v ->
        let
            p = getPrecision v
            x = v ! 0
            y = v ! 1
            a = mpBallP p 1
            b = mpBallP p 100
            amx  = a - x
            ymxs = y - x*x
        in
            amx^2 + b*ymxs^2
    )
    (symmetricDomain 2 (-1.2) 1.2)

himmelblau :: BoxFun
himmelblau =
    BoxFun
    2
    (\v ->
        let
            x = v ! 0
            y = v ! 1
            a = (x^2 + y - 11)
            b = (x + y^2 - 7)
        in
            a^2 + b^2
    )
    (symmetricDomain 2 (-600.0) 600.0)

shekel :: BoxFun
shekel = 
    BoxFun
    2
    (\v ->
        let
            x   = v ! 0
            y   = v ! 1
            c0  = 1;  a00 = 43; a01 = 23
            c1  = 17; a10 = 6;  a11 = 9
        in
            - 1/(c0 + (x - a00)^2 + (y - a01)^2)
            - 1/(c1 + (x - a10)^2 + (y - a11)^2)
    )
    (symmetricDomain 2 (-600.0) 600.0)

siam4 :: BoxFun
siam4 = 
    BoxFun
    2
    (\v ->
        let
            x   = v ! 0
            y   = v ! 1
        in
        exp(sin(50 * x)) + sin(60 * exp y) 
        + sin(70 * sin(x)) + sin(sin(80 * y)) 
        - sin(10 * (x + y)) 
        + (x^2 + y^2) / 4
    )
    (symmetricDomain 2 (-10.0) 10.0)

ratz4 :: BoxFun
ratz4 =
    BoxFun
    2
    (\v ->
        let 
            x  = v ! 0
            y  = v ! 1
            xs = x^2
            ys = y^2
        in 
            sin(xs + 2 * ys) * exp (-xs - ys)
    )
    (symmetricDomain 2 (-3.0) 3.0)

-- global minimum at schwefel(420.9687, 420.9687) ~ -837.9658
schwefel :: BoxFun
schwefel =
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            abs n = sqrt (n^2)
        in
            (-x * sin (sqrt (abs x))) + (-y * sin (sqrt (abs y)))
    )
    (symmetricDomain 2 (-500.0) 500.0)

-- global minimum at bukin(-10, 1) ~ 0
bukin :: BoxFun
bukin =
    BoxFun
    2
    (\v ->
        let
            x = v!0
            y = v!1
            abs n = sqrt (n^2)
        in
            100 * sqrt (abs (y - x^2 / 100)) + abs(x + 10) / 100
    )
    (symmetricDomain 2 (-15.0) 3.0) -- TODO: Make this -15 <= x <= 5, -3 <= y <= 3
