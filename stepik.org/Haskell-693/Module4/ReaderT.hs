--------------------------------------------------------------------------------
-- Demo.hs
-- $Date$
-- $Id$
-- $Version: 0.2$
-- $Revision: 4$
-- $Author: Victor |Stalker| Skurikhin <stalker@quake.ru>$
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module ReaderT where

import MonadTrans
import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f rdr = Reader $ f . runReader rdr

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  f <*> v = Reader $ \env -> runReader f env (runReader v env)

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  m >>= k = Reader $ \env ->
    let v = runReader m env
    in runReader (k v) env

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f rdr = ReaderT $ fmap f . runReaderT rdr

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure = ReaderT . const . pure
  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  f <*> v = ReaderT $ liftA2 (<*>) (runReaderT f) (runReaderT v)

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  m >>= k = ReaderT $ \env -> do -- вычисления в абстрактной внутренней монаде!!
    v <- runReaderT m env
    runReaderT (k v) env

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift m = ReaderT $ \_ -> m

ask :: Monad m => ReaderT r m r
ask = ReaderT return

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT $ return . f

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f rdr = ReaderT $ runReaderT rdr . f

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
