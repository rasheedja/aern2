{-|
    Module      :  Main (file aern2-fun-chPoly-benchOp)
    Description :  execute a ChPoly operation for benchmarking
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import MixedTypesNumPrelude
import qualified Prelude as P

import Text.Printf
import Text.Regex.TDFA

import System.Environment

-- import System.IO.Unsafe (unsafePerformIO)
-- import System.Random (randomRIO)
import System.Clock

import Data.List (isSuffixOf)

-- import Data.String (fromString)
-- import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Compression.GZip as GZip

-- import Test.QuickCheck

-- import AERN2.Utils.Bench

import AERN2.MP
-- import AERN2.Real

-- import AERN2.Interval

import qualified AERN2.Poly.Cheb as ChPoly
import AERN2.Poly.Cheb (ChPolyMB)

main :: IO ()
main =
  do
  args <- getArgs
  let (mode, op, serialisedFile, p, ac, count) = processArgs args
  runBenchmark mode op serialisedFile p ac count

data Mode = SummaryCSV | CSV | Verbose
  deriving (Show, Read)

processArgs :: [String] -> (Mode, String, String, Precision, Accuracy, Integer)
processArgs [modeS, op, serialisedFile, precS, acS, countS] =
  (read modeS, op, serialisedFile, readPrec precS, readAc acS, read countS)
  where
  readPrec = prec . read
  readAc "exact" = Exact
  readAc "any" = NoInformation
  readAc s = bits (read s :: Integer)
processArgs _ =
  error "expecting arguments: <mode> <operation> <serialisedFile> <precision> <accuracy> <count>"

loadSerialised :: String -> IO [(ChPolyMB, ChPolyMB)]
loadSerialised serialisedFile =
  do
  contentPre <- BS.readFile serialisedFile
  let content = BS.lines $ decompress contentPre
  -- print $ length content
  let fns = map deserialiseChPolyOrError content
  -- mapM_ print $ map getAccuracy fns
  return $ makePairs fns
  where
  makePairs (a:b:rest) = (a,b):makePairs rest
  makePairs _ = []
  deserialiseChPolyOrError s =
    case ChPoly.deserialise s of
      Just p -> p
      _ -> error $ "failed to deserialise: " ++ (BS.unpack s)
  decompress
    | ".gz" `isSuffixOf` serialisedFile = GZip.decompress
    | otherwise = id


runBenchmark :: Mode -> String -> String -> Precision -> Accuracy -> Integer -> IO ()
runBenchmark mode op serialisedFile p acGuide count =
  do
  tStart <- getTime ProcessCPUTime
  reportProgress tStart computationDescription

  reportProgress tStart "preparing arguments"
  valuePairs <- loadSerialised serialisedFile
  -- paramPairsPre <- pick valuePairs count
  let paramPairsPre = take count valuePairs

  let paramPairs =
        map (mapBoth (centreAsBall . setPrecision p)) paramPairsPre
  let paramAccuracies = concat $ map (\(a,b) -> [getAccuracy a, getAccuracy b]) paramPairs
  case minimum paramAccuracies of
    Exact -> pure ()
    ac -> putStrLn $ printf "An argument is not exact! (ac = %s)" (show ac)
  tGotParams <- getTime ProcessCPUTime

  reportProgress tGotParams $ "computing operation " ++ op
  let results = computeResults paramPairs
  tasResults <- mapM getResultCompDurationAndAccuracy $ zip [1..] results

  tDone <- getTime ProcessCPUTime
  reportProgress tDone $ "done"
  csvSummaryLine mode tStart tGotParams tasResults tDone

  where
  deg :: Integer
  deg = read degS
  (_,_,_,[degS]) = serialisedFile =~ "ChPolyPair-deg([0-9]*)-" :: (String, String, String, [String])
  computationDescription =
      printf "computing %s on ChPoly(s) (deg = %d, p = %s, count = %d samples)" op deg (show p) count
  computeResults paramPairs =
    case op of
      "add" -> map (uncurry (+)) paramPairs
      "mul" -> map (uncurry (*)) paramPairs
      "div" -> map (\(a,b) -> (~!) (ChPoly.chebDivideDCT acGuide a b )) paramPairs
      _ -> error $ "unknown op " ++ op
  getResultCompDurationAndAccuracy (i,result) =
    do
    tiResStart <- getTime ProcessCPUTime
    let ac = getAccuracy result
    reportProgress tiResStart $ printf "result %d accuracy = %s" i (show ac)
    tiResEnd <- seq ac $ getTime ProcessCPUTime
    csvLine mode i tiResStart tiResEnd ac
    return (tiResStart, ac)
  csvLine CSV i tResStart tResEnd ac =
    putStrLn $ printf "%s,%3d,%4d,%3d,%13.9f,%s"
                op deg (integer p) i
                (toSec dRes)
                (showAC ac)
    where
    dRes = tResEnd .-. tResStart
  csvLine _ _ _ _ _ = pure ()
  csvSummaryLine SummaryCSV tStart tGotParams tasResults tDone =
    putStrLn $ printf "%s,%3d,%4d,%4d,%16.9f,%13.9f,%13.9f,%13.9f,%s,%s"
                op deg (integer p) count
                (toSec dPrepParams)
                (toSec dGetResMean)
                (toSec dGetResWorst)
                (toSec dGetResStDev)
                (showAC acWorst)
                (showAC acBest)
    where
    (tsResults, acResults) = unzip tasResults
    acWorst = minimum acResults
    acBest = maximum acResults
    dPrepParams = tGotParams .-. tStart
    dsResults = zipWith (.-.) ((tail tsResults) ++ [tDone]) tsResults
    n = length tsResults
    dGetResMean = round $ (sum dsResults) /! n
    dGetResWorst = foldl1 max dsResults
    dGetResStDev =
      round $ (~!) $ sqrt $ double $
        (sum $ map (^!2) $ (map (\x -> x-dGetResMean) dsResults))
          /! (n - 1)
  csvSummaryLine _ _ _ _ _ = pure ()

  a .-. b = toNanoSecs $ diffTimeSpec a b
  toSec ns = (double ns) /! (10^!9)
  showAC Exact = "exact"
  showAC NoInformation = "noinformation"
  showAC ac = show $ fromAccuracy ac
  reportProgress now msg =
    case mode of
      Verbose ->
        printf "[%06d.%06d] ChPoly benchmark: %s\n" (sec now) (msec now) msg
      _ -> pure ()
    where
    msec time = nsec time `P.div` (P.fromInteger 1000)


mapBoth :: (t1 -> t2) -> (t1,t1) -> (t2,t2)
mapBoth f (a,b) = (f a, f b)
