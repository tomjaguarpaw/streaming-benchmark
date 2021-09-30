{-# LANGUAGE RankNTypes #-}

module Streaming.Codensity where

import qualified Streaming as S
import           Control.Monad.Codensity
import           Control.Monad.Trans.Class (lift, MonadTrans)

data Stream f m a = Stream { unStream :: Codensity (S.Stream f m) a }

instance Functor (Stream f m) where
  fmap f = Stream . fmap f . unStream

instance Applicative (Stream f m) where
  pure = return
  f <*> x = Stream (unStream f <*> unStream x)

instance Monad (Stream f m) where
  return = Stream . return
  m >>= f = Stream (unStream m >>= (unStream . f))

instance Functor f => MonadTrans (Stream f) where
  lift m = Stream (lift (lift m))

run :: Monad m => Stream m m r -> m r
run = S.run . lowerCodensity . unStream
