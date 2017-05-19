{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.QA.Strategy.Parallel
    Description :  QA net parallel evaluation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    QA net parallel evaluation
-}
module AERN2.QA.Strategy.Parallel
(
  QAParA
  , executeQAParA --, executeQAParUncachedA
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO  (\ (_ :: String)-> return ())
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P
import Text.Printf

import Control.Arrow

import qualified Data.IntMap as IntMap

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import AERN2.QA.Protocol

type QAParA = Kleisli QAParM

data QAParM a = QAParM { unQAParM :: IO a }

instance Functor QAParM where
  fmap f (QAParM tv2ma) = QAParM (fmap f tv2ma)
instance Applicative QAParM where
  pure a = QAParM (pure a)
  (QAParM tv2f) <*> (QAParM tv2a) = QAParM (tv2f <*> tv2a)
instance Monad QAParM where
  (QAParM tv2ma) >>= f = QAParM $ tv2ma >>= unQAParM . f
instance MonadIO QAParM where
  liftIO = QAParM

instance QAArrow QAParA where
  type QAId QAParA = ()
  qaRegister = Kleisli qaRegisterM
    where
    qaRegisterM qa@(QA__ name Nothing _sourceIds (p :: p) sampleQ _) =
      QAParM $
        do
        activeQsTV <- atomically $ newTVar initActiveQs
        cacheTV <- atomically $ newTVar $ newQACache p
        return $ QA__ name (Just ()) [] p sampleQ (Kleisli $ makeQPar activeQsTV cacheTV)
      where
      initActiveQs = IntMap.empty :: IntMap.IntMap (Q p)
      nextActiveQId activeQs
        | IntMap.null activeQs = int 1
        | otherwise =
            int $ 1 + (fst $ IntMap.findMax activeQs)
      makeQPar activeQsTV cacheTV q =
        QAParM $
          do
          maybeTraceIO $ printf "[%s]: q = %s" name (show q)
          -- consult the cache and index of active queries in an atomic transaction:
          (maybeAnswer, maybeComputeId) <- atomically $
            do
            cache <- readTVar cacheTV
            case lookupQACache p cache q of
              (Just a, _mLogMsg) -> return (Just a, Nothing)
              (_, _mLogMsg) ->
                do
                activeQs <- readTVar activeQsTV
                let alreadyActive = or $ map (!>=! q) $ IntMap.elems activeQs
                if alreadyActive then return (Nothing, Nothing) else
                  do
                  let computeId = nextActiveQId activeQs
                  writeTVar activeQsTV $ IntMap.insert computeId q activeQs
                  return (Nothing, Just computeId)
          -- act based on the cache and actity consultation:
          case (maybeAnswer, maybeComputeId) of
            (Just a, _) -> -- got cached answer, just return it:
              return $ promise (pure a)
            (_, Just computeId) ->
              -- no cached answer, no pending computation:
              do
              _ <- forkComputation computeId -- start a new computation
              return $ promise waitForAnwer -- and wait for the answer
            _ -> -- no cached answer but there is a pending computation:
              return $ promise waitForAnwer -- wait for a pending computation
        where
        promise io = Kleisli $ const $ QAParM io
        waitForAnwer = atomically $
          do
          cache <- readTVar cacheTV
          case lookupQACache p cache q of
            (Just a, _mLogMsg) -> return a
            (_, _mLogMsg) -> retry
        forkComputation computeId =
          forkIO $
            do
            -- compute an answer:
            a <- unQAParM $ runKleisli (qaMakeQuery qa) q
            -- update the cache with this answer:
            atomically $ modifyTVar cacheTV (updateQACache p q a)
            -- remove computeId from active queries:
            atomically $ modifyTVar activeQsTV (IntMap.delete computeId)
    qaRegisterM _ =
      error "internal error in AERN2.QA.Strategy.Par: qaRegister called with an existing id"
--
  qaFulfilPromiseA = Kleisli qaFulfilPromiseM
    where
    qaFulfilPromiseM promiseA =
      runKleisli promiseA ()
  qaMakeQueryGetPromiseA = Kleisli qaMakeQueryGetPromiseM
    where
    qaMakeQueryGetPromiseM (qa, q) =
      runKleisli (qaMakeQueryGetPromise qa) q
--
executeQAParA :: (QAParA () a) -> IO a
executeQAParA code = unQAParM $ runKleisli code ()
