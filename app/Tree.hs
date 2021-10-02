{-# LANGUAGE LambdaCase #-}

module Tree where

import           Control.Monad.Trans.Class (lift, MonadTrans)
import           Control.Monad.Trans.Identity (runIdentityT)

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

walkTreeIO :: (Int -> IO ()) -> Tree -> IO ()
walkTreeIO doSomething = loop
  where loop = \case
          Leaf i -> doSomething i
          Branch t1 t2 -> do
            loop t1
            loop t2

walkTreeList :: (Int -> IO ()) -> Tree -> IO ()
walkTreeList doSomething = Prelude.mapM_ doSomething . toList
  where toList = \case
          Leaf i -> [i]
          Branch t1 t2 -> toList t1 ++ toList t2

walkTreeTransformer :: (Monad (m IO), MonadTrans m)
                    => (Int -> IO ()) -> Tree -> m IO ()
walkTreeTransformer doSomething = loop
  where loop = \case
          Leaf i -> lift (doSomething i)
          Branch t1 t2 -> do
            loop t1
            loop t2

walkTreeIdentityT :: (Int -> IO ()) -> Tree -> IO ()
walkTreeIdentityT doSomething = runIdentityT . walkTreeTransformer doSomething

walkTreeStreaming :: (Int -> IO ()) -> Tree -> IO ()
walkTreeStreaming doSomething = Streaming.run . walkTreeTransformer doSomething

walkTreeBetterStreaming :: (Int -> IO ()) -> Tree -> IO ()
walkTreeBetterStreaming doSomething = Streaming.Better.run . walkTreeTransformer doSomething

walkTreeStreamingCodensity :: (Int -> IO ()) -> Tree -> IO ()
walkTreeStreamingCodensity doSomething = Streaming.Codensity.run . walkTreeTransformer doSomething

walkTreeStreamly :: (Int -> IO ()) -> Tree -> IO ()
walkTreeStreamly doSomething = Streamly.Prelude.drain . walkTreeTransformer doSomething

walkTreePipes :: (Int -> IO ()) -> Tree -> IO ()
walkTreePipes doSomething = Pipes.runEffect . walkTreeTransformer doSomething

walkTreeConduit :: (Int -> IO ()) -> Tree -> IO ()
walkTreeConduit doSomething = Data.Conduit.runConduit . walkTreeTransformer doSomething
