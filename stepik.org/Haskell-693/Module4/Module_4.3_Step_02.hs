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

demo4'3'2 :: [Either String Int]
demo4'3'2 = runExceptT (except $ Right 42)

--------------------------------------------------------------------------------
-- vim: syntax=haskell:fileencoding=utf-8:ff=unix:tw=78:ts=4:sw=4:sts=4:et
{-EOF-}
