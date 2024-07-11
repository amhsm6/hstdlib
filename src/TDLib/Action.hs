{-# LANGUAGE FlexibleInstances #-}

module TDLib.Action
    ( Action, runAction
    , unwrap, Checkable, check
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.STM.TQueue
import qualified TD.Lib as TDL
import qualified TD.GeneralResult as TDL

type Action = ReaderT (TDL.Client, TQueue TDL.GeneralResult) (ExceptT () IO)

runAction :: Action a -> TDL.Client -> TQueue TDL.GeneralResult -> IO (Maybe a)
runAction m client q = either (const Nothing) Just <$> runExceptT (runReaderT m (client, q))

unwrap :: Maybe a -> Action a
unwrap = mapError $ maybe mzero pure

class Checkable a where
    check :: a -> Action ()

instance Checkable Bool where
    check = guard

instance Checkable (Maybe Bool) where
    check = unwrap >=> guard
