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


printStdErr :: Show a => a -> IO ()
printStdErr = IO.hPutStrLn IO.stderr . show

-- Fully evaluated left-skewed tree
leftSkewed :: Int -> Tree
leftSkewed 0 = Leaf 0
leftSkewed n = (Branch $! leftSkewed (n - 1)) (Leaf n)

printTreeIO :: IORef Int -> Tree -> IO ()
printTreeIO r = \case
  Leaf i -> writeIORef r i
  Branch t1 t2 -> do
    printTreeIO r t1
    printTreeIO r t2

printTreeList :: IORef Int -> Tree -> IO ()
printTreeList r = Prelude.mapM_ (writeIORef r) . toList
  where toList = \case
          Leaf i -> [i]
          Branch t1 t2 -> toList t1 ++ toList t2

printTreeTransformer :: (Monad (m IO), MonadTrans m)
                     => IORef Int -> Tree -> m IO ()
printTreeTransformer r = \case
  Leaf i -> lift (writeIORef r i)
  Branch t1 t2 -> do
    printTreeTransformer r t1
    printTreeTransformer r t2

printTreeIdentityT :: IORef Int -> Tree -> IO ()
printTreeIdentityT r = runIdentityT . printTreeTransformer r

printTreeStreaming :: IORef Int -> Tree -> IO ()
printTreeStreaming r = Streaming.run . printTreeTransformer r

printTreeBetterStreaming :: IORef Int -> Tree -> IO ()
printTreeBetterStreaming r = Streaming.Better.run . printTreeTransformer r

printTreeStreamingCodensity :: IORef Int -> Tree -> IO ()
printTreeStreamingCodensity r = Streaming.Codensity.run . printTreeTransformer r

printTreeStreamly :: IORef Int -> Tree -> IO ()
printTreeStreamly r = Streamly.Prelude.drain . printTreeTransformer r

printTreePipes :: IORef Int -> Tree -> IO ()
printTreePipes r = Pipes.runEffect . printTreeTransformer r

printTreeConduit :: IORef Int -> Tree -> IO ()
printTreeConduit r = Data.Conduit.runConduit . printTreeTransformer r
