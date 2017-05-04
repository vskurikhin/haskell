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

demo4'3'3'1 :: [Either String Int]
demo4'3'3'1 = runExceptT (fmap (+9) $ except $ Right 42)

demo4'3'3'2 :: [Either String Int]
demo4'3'3'2 = runExceptT (fmap (+9) $ except $ Left "ABC")

demo4'3'3'3 :: [Either String Int]
demo4'3'3'3 = runExceptT (fmap undefined $ except $ Left "ABC")

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
