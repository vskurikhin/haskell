--------------------------------------------------------------------------------
-- Demo.hs
-- $Date$
-- $Id$
-- $Version: 0.1$
-- $Revision: 3$
-- $Author: Victor |Stalker| Skurikhin <stalker@quake.ru>$
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module Demo where

import MonadTrans
import ReaderT
import WriterT
import StateT
import Control.Monad.Identity
import Control.Applicative (liftA2)
import Data.Char (toUpper)

newtype Except e a = Except { runExcept :: Either e a }
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . return

instance Functor (Except e) where
  fmap :: (a -> b) -> Except e a -> Except e b
  fmap f = Except . fmap f . runExcept

instance Functor m => Functor (ExceptT e m) where
  fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance Applicative (Except e) where
  pure :: Applicative (Except e) => a -> Except e a
  pure = Except . Right
  (<*>) :: Applicative (Except e) => Except e (a -> b) -> Except e a -> Except e b
  f <*> v = Except $ (runExcept f) <*> (runExcept v) where

instance Applicative m => Applicative (ExceptT e m) where
  pure :: Applicative (ExceptT e m) => a -> ExceptT e m a
  pure = ExceptT . pure . Right
  (<*>) :: Applicative (ExceptT e m) => ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
  f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v) where

instance Monad (Except e) where
  (>>=) :: Monad (Except e) => Except e a -> (a -> Except e b) -> Except e b
  m >>= k = Except $
    case runExcept m of
      Left e  -> Left e
      Right x -> runExcept (k x)

instance (Monad m) => Monad (ExceptT e m) where
  (>>=) :: Monad (ExceptT e m) => ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
  m >>= k = ExceptT $ do
    a <- runExceptT m
    case a of
      Left e  -> return (Left e)
      Right x -> runExceptT (k x)
  fail = ExceptT . fail

instance  MonadTrans (ExceptT e) where
  lift :: Monad m =>  m a -> ExceptT e m a
  lift = ExceptT . fmap Right

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h = ExceptT $ do
  a <- runExceptT m
  case a of
    Left  l -> runExceptT (h l)
    Right r -> return (Right r)

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
