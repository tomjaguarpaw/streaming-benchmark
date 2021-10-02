{-# LANGUAGE LambdaCase #-}

module Tree where

import           Control.Monad.Trans.Class (lift, MonadTrans)
import           Control.Monad.Trans.Identity (runIdentityT)
import           Data.IORef (writeIORef, IORef)
import qualified System.IO as IO

import qualified Data.Conduit
import qualified Streaming
import qualified Streaming.Better
import qualified Streaming.Codensity
import qualified Streamly.Prelude
import qualified Pipes

data Tree = Branch Tree Tree | Leaf Int

-- Fully evaluated left-skewed tree
leftSkewed :: Int -> Tree
leftSkewed 0 = Leaf 0
leftSkewed n = (Branch $! leftSkewed (n - 1)) (Leaf n)

walkTreeIO :: IORef Int -> Tree -> IO ()
walkTreeIO r = \case
  Leaf i -> writeIORef r i
  Branch t1 t2 -> do
    walkTreeIO r t1
    walkTreeIO r t2

walkTreeList :: IORef Int -> Tree -> IO ()
walkTreeList r = Prelude.mapM_ (writeIORef r) . toList
  where toList = \case
          Leaf i -> [i]
          Branch t1 t2 -> toList t1 ++ toList t2

walkTreeTransformer :: (Monad (m IO), MonadTrans m)
                     => IORef Int -> Tree -> m IO ()
walkTreeTransformer r = \case
  Leaf i -> lift (writeIORef r i)
  Branch t1 t2 -> do
    walkTreeTransformer r t1
    walkTreeTransformer r t2

walkTreeIdentityT :: IORef Int -> Tree -> IO ()
walkTreeIdentityT r = runIdentityT . walkTreeTransformer r

walkTreeStreaming :: IORef Int -> Tree -> IO ()
walkTreeStreaming r = Streaming.run . walkTreeTransformer r

walkTreeBetterStreaming :: IORef Int -> Tree -> IO ()
walkTreeBetterStreaming r = Streaming.Better.run . walkTreeTransformer r

walkTreeStreamingCodensity :: IORef Int -> Tree -> IO ()
walkTreeStreamingCodensity r = Streaming.Codensity.run . walkTreeTransformer r

walkTreeStreamly :: IORef Int -> Tree -> IO ()
walkTreeStreamly r = Streamly.Prelude.drain . walkTreeTransformer r

walkTreePipes :: IORef Int -> Tree -> IO ()
walkTreePipes r = Pipes.runEffect . walkTreeTransformer r

walkTreeConduit :: IORef Int -> Tree -> IO ()
walkTreeConduit r = Data.Conduit.runConduit . walkTreeTransformer r
