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

module StateT where

import MonadTrans
import ReaderT
import WriterT
import Control.Monad.Identity
import Data.Char (toUpper)
import Control.Applicative


newtype State s a = State { runState :: s -> (a, s) }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f m = State $ \st -> updater $ runState m st
    where updater ~(x, s) = (f x, s)

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)


instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \ s -> (x, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  f <*> v = State $ \ s -> let
       (g, s')  = runState f s
       (x, s'') = runState v s'
      in (g x, s'')

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \ s -> return (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')


instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  m >>= k  = State $ \s -> let
     (x, s') = runState m s
    in runState (k x) s'

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'


instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \st -> do
    a <- m
    return (a, st)

get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT st = fmap fst . runStateT st

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT st = fmap snd . runStateT st

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
