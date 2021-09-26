{-# LANGUAGE LambdaCase #-}

module Tree where

import qualified System.IO as IO
import qualified Streaming
import qualified Streaming.Better
import           Control.Monad.Trans.Class (lift)

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
