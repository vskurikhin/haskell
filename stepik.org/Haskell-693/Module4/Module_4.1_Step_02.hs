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

{-
writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT
--}

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

demo4'1'2'1 = runWriterT (WriterT $ Just (42,"Hello!"))
demo4'1'2'2 = runWriterT (writer $  (42,"Hello!")) :: [(Int,String)]

execWriter :: Writer w a -> w
execWriter = snd . runWriter

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
