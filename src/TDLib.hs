module TDLib
    ( start, auth
    , get, put
    , send
    , module TDLib.Action
    , module TD.GeneralResult
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Text as T
import Data.Aeson (ToJSON)
import System.Environment
import qualified TD.Lib as TDL
import TD.GeneralResult
import TD.Data.Update
import TD.Data.AuthorizationState
import TD.Query.SetTdlibParameters
import TD.Query.SetAuthenticationPhoneNumber
import TD.Query.CheckAuthenticationCode

import TDLib.Action

send :: ToJSON a => a -> Action ()
send x = ask >>= \(client, _) -> liftIO $ TDL.send client x

recv :: Action (Maybe (GeneralResult, Maybe TDL.Extra))
recv = ask >>= \(client, _) -> liftIO $ TDL.receive client

get :: Action GeneralResult
get = ask >>= liftIO . atomically . readTQueue . snd

put :: GeneralResult -> Action ()
put x = ask >>= \(_, q) -> liftIO $ atomically $ writeTQueue q x

recvloop :: Action a
recvloop = forever $ tryError $ do
    recv >>= unwrap >>= put . fst
    liftIO $ threadDelay 1000

start :: Action a -> IO ()
start m = do
    client <- TDL.create 
    q <- atomically newTQueue

    forkIO $ void $ runAction recvloop client q
    void $ runAction m client q

auth :: Action ()
auth = do
    x <- get
    case x of
        Update (UpdateAuthorizationState (Just AuthorizationStateReady)) -> pure ()
        Update (UpdateAuthorizationState (Just state)) -> exec state >> auth
        _ -> put x >> auth

exec :: AuthorizationState -> Action ()
exec AuthorizationStateReady = pure ()
exec AuthorizationStateWaitTdlibParameters = do
    apiId <- liftIO $ getEnv "API_ID"
    apiHash <- liftIO $ getEnv "API_HASH"
    send $ defaultSetTdlibParameters { database_directory = Just $ T.pack "db"
                                     , api_id = Just $ read apiId
                                     , api_hash = Just $ T.pack apiHash
                                     , device_model = Just $ T.pack "Haskell"
                                     , system_language_code = Just $ T.pack "en"
                                     , application_version = Just $ T.pack "1.0.0"
                                     }
exec AuthorizationStateWaitPhoneNumber = do
    phone <- liftIO $ getEnv "PHONE_NUMBER"
    send $ defaultSetAuthenticationPhoneNumber { phone_number = Just $ T.pack phone }
exec x@(AuthorizationStateWaitCode _) = do
    liftIO $ putStrLn "Type authentication code"
    code <- liftIO $ getLine
    when (null code) $ liftIO (putStrLn "Wrong code") >> exec x
    send $ CheckAuthenticationCode { code = Just $ T.pack code }
exec x = do
    liftIO $ putStrLn $ concat [ "Unknown Authorization State:\n"
                               , show x
                               , "\n --> Skipping"
                               ]
    mzero
