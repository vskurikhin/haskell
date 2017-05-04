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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module WriterT where

import Control.Applicative (liftA2)
import Data.Tuple (swap)

newtype Writer w a = Writer { runWriter :: (a, w) }
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

demo4'1'1 = runWriterT (WriterT $ Just (42,"Hello!"))

{-
writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT
--}

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

execWriter :: Writer w a -> w
execWriter = snd . runWriter

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT

instance Functor (Writer w) where
  fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f = Writer . updater . runWriter where
    updater ~(x, log) = (f x, log)

instance Functor m => Functor (WriterT w m) where
  fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
  fmap f = WriterT . fmap updater . runWriterT where
    updater ~(x, log) = (f x, log)

demo4'1'3'1 = runWriter (fmap (^2) $ Writer (3,"A"))
{-
*WriterT> runWriter (fmap (^2) $ Writer (3,"A"))
(9,"A")
--}

demo4'1'3'2 = runWriterT (fmap (^2) $ WriterT [(3,"A")])
{-
*WriterT> runWriterT (fmap (^2) $ WriterT [(3,"A")])
[(9,"A")]
--}

demo4'1'3'3 = runWriterT (fmap (^2) $ WriterT [(3,"A"), (4,"B")])
{-
*WriterT> runWriterT (fmap (^2) $ WriterT [(3,"A"), (4,"B")])
[(9,"A")]
--}

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
