{-# LANGUAGE LambdaCase #-}

module Tree where

import           Control.Monad.Trans.Class (lift, MonadTrans)
import           Control.Monad.Trans.Identity (runIdentityT)
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

printTreeTransformer :: (Monad (m IO), MonadTrans m) => Tree -> m IO ()
printTreeTransformer = \case
  Leaf i -> lift (printStdErr i)
  Branch t1 t2 -> do
    printTreeTransformer t1
    printTreeTransformer t2

printTreeIdentityT :: Tree -> IO ()
printTreeIdentityT = runIdentityT . printTreeTransformer

printTreeStreaming :: Tree -> IO ()
printTreeStreaming = Streaming.run . printTreeTransformer

printTreeBetterStreaming :: Tree -> IO ()
printTreeBetterStreaming = Streaming.Better.run . printTreeTransformer

printTreeStreamly :: Tree -> IO ()
printTreeStreamly = Streamly.Prelude.drain . printTreeTransformer

printTreePipes :: Tree -> IO ()
printTreePipes = Pipes.runEffect . printTreeTransformer

printTreeConduit :: Tree -> IO ()
printTreeConduit = Data.Conduit.runConduit . printTreeTransformer
