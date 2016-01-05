{-# LANGUAGE Arrows, TypeOperators #-}
module AERN2.Net.Examples.FFT 
(
    fftTestDirect,
    dftCooleyTukey
)
where

import AERN2.Num

import AERN2.Net.Spec.Arrow
import Control.Arrow

import AERN2.Net.Execution.Direct ()
import AERN2.Net.Execution.QACached

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

fftTestDirect :: Integer -> Accuracy -> [(MPBall, MPBall)]
fftTestDirect nN ac =
    map (\ c -> complexCR2balls c ac) $ fftWithInput ()
    where
    fftWithInput =
        proc () ->
            do
            x <- complexListNamedA "input" -< input
            dftCooleyTukey nN -< x
    input = map rational [1..nN] 

{- TODO
fftTestCached :: Integer -> Accuracy -> [(MPBall, MPBall)]
fftTestCached nN ac =
    executeQACachedM $
        do
        rs <- runKleisli (fftWithInput :: QACachedA () [QACached_ComplexCR]) ()
        anss <- mapM (\(QACached_ComplexCR rId) -> getAnswer QAP_ComplexCR rId ac) rs
        return anss
    where
    fftWithInput =
        proc () ->
            do
            x <- complexListNamedA "input" -< input
            dftCooleyTukey nN -< x
    input = map rational [1..nN] 
-}

{-|
    Discrete Fourier Transform using the Cooley and Tukey Radix-2 algorithm.
    
    Preconditions:
    
    * @N > 0@ is a power of 2.
    
    Arrow preconditions:
    
    * The input list has exactly @N@ elements.
-}
dftCooleyTukey :: 
    (ComplexA to c)
    =>
    Integer {-^ @N@ -} -> 
    [c] `to` [c]
dftCooleyTukey nN = ditfft2 nN 1

{-|
    Radix-2 Cooley-Tukey as described in:
    
    https://en.wikipedia.org/wiki/Cooley-Tukey_FFT_algorithm#The_radix-2_DIT_case
    
    Preconditions:
    
    * @N > 0@ is a power of 2.
    * @s > 0@
    
    Arrow preconditions:
    
    * The input list has at least @s*(N-1) + 1@ elements.
-}
ditfft2 :: 
    (ComplexA to c)
    =>
    Integer {-^ @N@ -} -> 
    Integer {-^ @s@ -} ->
    [c] `to` [c]
ditfft2 nN s
    | nN == 1 =
        proc (x0:_) -> 
            returnA -< [x0]
    | otherwise =
        proc x ->
            do
            vTX0 <- ditfft2 nNhalf (2 * s) -< x 
            vTXNhalf <- ditfft2 nNhalf (2 * s) -< (drop (int s) x)
            vTXNhalfTwiddled <- mapA twiddle -< vTXNhalf
            vX0 <- zipWithA (const addA) -< (vTX0, vTXNhalfTwiddled)
            vXNhalf <- zipWithA (const subA) -< (vTX0, vTXNhalfTwiddled)
            returnA -< vX0 ++ vXNhalf
    where
    nNhalf = round (nN / 2)
    twiddle k = 
        proc x_k_plus_NHalf ->
            do
            tc <- convertNamedA "exp(-2*pi*i*k/nN)*" -< cT 
            r <- mulA -< (x_k_plus_NHalf, tc)
            let _ = [tc,r,x_k_plus_NHalf]
            returnA -< r
        where
        cT = 
            maybeTrace
            (
                "twiddle with k = " ++ show k ++ "; ... = " ++ showComplexCR (bits 100) c
            )
            c
        c = exp(-2*pi*i*k/nN)
        i = complexI :: Complex CauchyReal
    
                