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
  pure = Except . Right
  f <*> v = Except $ (runExcept f) <*> (runExcept v) where
--f <*> v = Except $ updater (runExcept f) (runExcept v) where
--  updater (Left e)  _ = Left e
--  updater (Right g) x = fmap g x

instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v) where
--f <*> v = ExceptT $ liftA2 updater (runExceptT f) (runExceptT v) where
--  updater (Left e)  _ = Left e
--  updater (Right g) x = fmap g x

demo4'3'4'1 :: [Either String Int]
demo4'3'4'1 = runExceptT $ (except $ Right (^2)) <*> (except $ Right 3)

demo4'3'4'2 :: [Either String Int]
demo4'3'4'2 = runExceptT $ (except $ Left "ABC") <*> (except $ Right 3)

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
