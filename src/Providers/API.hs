module Providers.API
( Endpoint
, ProviderEndpoint(..)
, withNewEndpoint
, publishMessage
, onMessageReceived
, awaitMessageReceived
, awaitMessageDispatched
) where

import Core
import Control.Concurrent.STM

data Endpoint = Endpoint
  { sender :: TQueue (Message, ChannelId)
  , receiver :: TQueue Message
  }

newEndpoint :: IO Endpoint
newEndpoint = atomically $ Endpoint <$> newTQueue <*> newTQueue

publishMessage :: Endpoint -> Message -> ChannelId -> IO ()
publishMessage endpoint message targetChannelId = atomically $ writeTQueue (sender endpoint) (message, targetChannelId)

onMessageReceived :: Endpoint -> Message -> IO ()
onMessageReceived endpoint = atomically . writeTQueue (receiver endpoint)

awaitMessageReceived :: Endpoint -> IO Message
awaitMessageReceived = atomically . readTQueue . receiver

awaitMessageDispatched :: Endpoint -> IO (Message, ChannelId)
awaitMessageDispatched = atomically . readTQueue . sender

withNewEndpoint :: (Endpoint -> IO ()) -> IO Endpoint
withNewEndpoint f = do
  endpoint <- newEndpoint
  f endpoint
  return endpoint

class ProviderEndpoint config where
  spawnProviderEndpoint :: config -> IO Endpoint
