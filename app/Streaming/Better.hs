{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

module Streaming.Better where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.Class (lift, MonadTrans)

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

instance Functor f => MonadTrans (Stream f) where
  lift = Effect . fmap Return

data Of a b = !a :> b deriving Functor

yield :: Monad m => a -> Stream (Of a) m ()
yield a = Step (a :> Return ())

{-| Run the effects in a stream that merely layers effects.
-}
run :: Monad m => Stream m m r -> m r
run = loop where
  loop stream = case stream of
    Return r   -> return r
    Effect  m  -> m >>= loop
    Step mrest -> mrest >>= loop
    Bind (Return n) k -> loop (k n)
    Bind (Effect m) k -> do
      r <- m
      loop (Bind r k)
    Bind (Step mrest) k -> do
      r <- mrest
      loop (Bind r k)
    Bind (Bind m k1) k2 -> loop (Bind m (k1 >=> k2))

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
