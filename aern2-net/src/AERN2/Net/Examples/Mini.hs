module AERN2.Net.Examples.Mini 
    (module AERN2.Net.Examples.Mini,
     module AERN2.Num)
where

import AERN2.Num

import Control.Arrow

import AERN2.Net.Execution.QACached

{--- a simple complex expression, a part of FFT ---}

twiddle :: (Integer, Integer) -> Complex CauchyReal
twiddle(k,n) =  exp(-2*k*complex_i*pi/n)

twiddleA :: (RealPredA to r) => (Integer, Integer) -> () `to` (Complex r)
twiddleA(k,n) = $(exprA[| let [i]=vars in exp(-2*k*i*pi/n)|]) <<< complex_iA

{--- the logistic map ---}

logisticNoA :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticNoA c n =
    foldl1 (.) (replicate (int n) step)
    where
    step x = c * x * (1 - x)

logisticA :: (RealExprA to r) => Rational -> Integer -> r `to` r
logisticA c n =
    foldl1 (<<<) (replicate (int n) step) 
    where
    step = $(exprA[|let [x]=vars in  c * x * (1 - x)|])
    
logisticQACached :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticQACached c n x0 =
    newCRA ([], Nothing, ac2ball)
    where
    ac2ball ac =
        snd $ logisticQACachedMPBall c n x0 ac
            
logisticQACachedMPBall :: Rational -> Integer -> CauchyReal -> (Accuracy -> (QANetLog, MPBall))
logisticQACachedMPBall c n x0 ac =
    executeQACachedA auxA
    where
    auxA =
        proc () ->
            do
            r <- logisticA c n <<< convertA -< x0
            getAnswerCRA -< (r :: QACached_CauchyReal,ac)
    
logisticQACachedMPBallPrintLog :: Rational -> Integer -> CauchyReal -> Accuracy -> IO ()
logisticQACachedMPBallPrintLog c n x0 ac =
    printQANetLogThenResult (logisticQACachedMPBall c n x0 ac)
            
logisticMPBIterate :: Rational -> Integer -> CauchyReal -> CauchyReal
logisticMPBIterate c n x0 =
    newCRA ([], Nothing, ac2ball)
    where
    ac2ball ac =
        snd $ last $ iterateUntilAccurate ac auxP  
    auxP p = 
        logisticA c n x0p
        where
        x0p = cauchyReal2ball x0 ac
        ac = bits $ prec2integer p
