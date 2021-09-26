{-# LANGUAGE LambdaCase #-}

module Tree where

import           Control.Monad.Trans.Class (lift)
import qualified System.IO as IO

import qualified Data.Conduit
import qualified Streaming
import qualified Streaming.Better
import qualified Streamly.Prelude
import qualified Pipes

data Tree = Branch Tree Tree | Leaf Int


printStdErr :: Show a => a -> IO ()
printStdErr = IO.hPutStrLn IO.stderr . show

-- Fully evaluated left-skewed tree
leftSkewed :: Int -> Tree
leftSkewed 0 = Leaf 0
leftSkewed n = (Branch $! leftSkewed (n - 1)) (Leaf n)

printTreeIO :: Tree -> IO ()
printTreeIO = \case
  Leaf i -> printStdErr i
  Branch t1 t2 -> do
    printTreeIO t1
    printTreeIO t2

printTreeList :: Tree -> IO ()
printTreeList = Prelude.mapM_ printStdErr . toList
  where toList = \case
          Leaf i -> [i]
          Branch t1 t2 -> toList t1 ++ toList t2

printTreeStreaming :: Tree -> IO ()
printTreeStreaming = Streaming.run . toStream
  where toStream = \case
          Leaf i -> lift (printStdErr i)
          Branch t1 t2 -> do
            toStream t1
            toStream t2

printTreeBetterStreaming :: Tree -> IO ()
printTreeBetterStreaming = Streaming.Better.run . toStream
  where toStream = \case
          Leaf i -> lift (printStdErr i)
          Branch t1 t2 -> do
            toStream t1
            toStream t2

printTreeStreamly :: Tree -> IO ()
printTreeStreamly = Streamly.Prelude.drain . toStream
  where toStream = \case
          Leaf i -> lift (printStdErr i)
          Branch t1 t2 -> do
            toStream t1
            toStream t2

printTreePipes :: Tree -> IO ()
printTreePipes = Pipes.runEffect . toStream
  where toStream = \case
          Leaf i -> lift (printStdErr i)
          Branch t1 t2 -> do
            toStream t1
            toStream t2

printTreeConduit :: Tree -> IO ()
printTreeConduit = Data.Conduit.runConduit . toStream
  where toStream = \case
          Leaf i -> lift (printStdErr i)
          Branch t1 t2 -> do
            toStream t1
            toStream t2
