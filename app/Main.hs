{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified System.Clock as Clock
import qualified System.IO as IO
import qualified Streaming.Prelude
import           Control.Monad ((>=>))

data Tree = Branch Tree Tree | Leaf Int


data Stream f m r = Step !(f (Stream f m r))
                  | forall a. Bind (Stream f m a) (a -> Stream f m r)
                  | Effect (m (Stream f m r))
                  | Return r

instance (Functor f, Monad m) => Functor (Stream f m) where
  fmap f = loop where
    loop stream = case stream of
      Return r -> Return (f r)
      Effect m -> Effect (do {stream' <- m; return (loop stream')})
      Step   g -> Step (fmap loop g)
      Bind s k -> Bind s (fmap loop k)
  a <$ stream0 = loop stream0 where
    loop stream = case stream of
      Return _ -> Return a
      Effect m -> Effect (do {stream' <- m; return (loop stream')})
      Step   f -> Step (fmap loop f)
      Bind s k -> Bind s (fmap loop k)

instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure = Return
  streamf <*> streamx = do {f <- streamf; x <- streamx; return (f x)}
  stream1 *> stream2 = Bind stream1 (const stream2)

instance (Functor f, Monad m) => Monad (Stream f m) where
  return = pure
  (>>) = (*>)

  stream >>= f = Bind stream f
{-
    loop stream where
    loop stream0 = case stream0 of
      Step fstr -> Step (fmap loop fstr)
      Effect m  -> Effect (fmap loop m)
      Return r  -> f r
-}

data Of a b = !a :> b deriving Functor

yield :: Monad m => a -> Stream (Of a) m ()
yield a = Step (a :> Return ())

mapM_ :: Monad m => (a -> m x) -> Stream (Of a) m r -> m r
mapM_ f = loop where
  loop str = case str of
    Return r -> return r
    Effect m -> m >>= loop
    Step (a :> as) -> f a *> loop as
    Bind (Return n) k -> loop (k n)
    Bind (Effect m) k -> do
      r <- m
      loop (Bind r k)
    Bind (Step (a :> as)) k -> f a *> loop (Bind as k)
    Bind (Bind m k1) k2 -> loop (Bind m (k1 >=> k2))

-- Fully evaluated left-skewed tree
leftSkewed :: Int -> Tree
leftSkewed 0 = Leaf 0
leftSkewed n = (Branch $! leftSkewed (n - 1)) (Leaf n)

printTreeIO :: Tree -> IO ()
printTreeIO = \case
  Leaf i -> IO.hPutStrLn IO.stderr (show i)
  Branch t1 t2 -> do
    printTreeIO t1
    printTreeIO t2

printTreeList :: Tree -> IO ()
printTreeList = Prelude.mapM_ (IO.hPutStrLn IO.stderr . show) . toList
  where toList = \case
          Leaf i -> [i]
          Branch t1 t2 -> toList t1 ++ toList t2

printTreeStreaming :: Tree -> IO ()
printTreeStreaming = Streaming.Prelude.mapM_ (IO.hPutStrLn IO.stderr . show) . toStream
  where toStream = \case
          Leaf i -> Streaming.Prelude.yield i
          Branch t1 t2 -> do
            toStream t1
            toStream t2

printTreeBetterStreaming :: Tree -> IO ()
printTreeBetterStreaming = Main.mapM_ (IO.hPutStrLn IO.stderr . show) . toStream
  where toStream = \case
          Leaf i -> Main.yield i
          Branch t1 t2 -> toStream t1 >> toStream t2

main :: IO ()
main = do
  let !mil = leftSkewed (2000)
      !tenmil = leftSkewed (20 * 1000)

  print "IO"
  startmil <- Clock.getTime Clock.Monotonic
  printTreeIO mil
  stopmil <- Clock.getTime Clock.Monotonic

  print (stopmil - startmil)

  starttenmil <- Clock.getTime Clock.Monotonic
  printTreeIO tenmil
  stoptenmil <- Clock.getTime Clock.Monotonic

  print (stoptenmil - starttenmil)

  print "List"
  startmil <- Clock.getTime Clock.Monotonic
  printTreeList mil
  stopmil <- Clock.getTime Clock.Monotonic

  print (stopmil - startmil)

  starttenmil <- Clock.getTime Clock.Monotonic
  printTreeList tenmil
  stoptenmil <- Clock.getTime Clock.Monotonic

  print (stoptenmil - starttenmil)

  print "Streaming"
  startmil <- Clock.getTime Clock.Monotonic
  printTreeStreaming mil
  stopmil <- Clock.getTime Clock.Monotonic

  print (stopmil - startmil)

  starttenmil <- Clock.getTime Clock.Monotonic
  printTreeStreaming tenmil
  stoptenmil <- Clock.getTime Clock.Monotonic

  print (stoptenmil - starttenmil)

  print "Better streaming"
  startmil <- Clock.getTime Clock.Monotonic
  printTreeBetterStreaming mil
  stopmil <- Clock.getTime Clock.Monotonic

  print (stopmil - startmil)

  starttenmil <- Clock.getTime Clock.Monotonic
  printTreeBetterStreaming tenmil
  stoptenmil <- Clock.getTime Clock.Monotonic

  print (stoptenmil - starttenmil)
