module Providers.Discord where

import Core
import Config
import Control.Concurrent.Async.Lifted
import Control.Monad
import Data.Text as T
import Discord
import qualified Discord.Types as DT
import qualified Discord.Requests as DR
import Providers.API
import Control.Monad.IO.Class

instance ProviderEndpoint DiscordConfig where
  spawnProviderEndpoint DiscordConfig {..} = withNewEndpoint $ \endpoint -> void . async . runDiscord $ def
    { discordToken   = token
    , discordOnStart = spawnSender endpoint
    , discordOnEvent = eventHandler endpoint
    }
    where
      eventHandler :: Endpoint -> DT.Event -> DiscordHandler ()
      eventHandler endpoint (DT.MessageCreate m) = do
        liftIO $ putStrLn "Handling Discord event"
        Right channelName <- fmap DT.channelName <$> getChannel cid
        let channel = Channel channelId channelName
        when (content /= "") $ onMessageReceived endpoint Message { .. } -- ignore images for now
        where
          user       = UserRef . DT.userName . DT.messageAuthor $ m
          cid        = DT.messageChannel m
          channelId  = ChannelId $ T.pack . show $ cid
          content    = DT.messageText m
          getChannel = restCall . DR.GetChannel
      eventHandler _ _ = return ()

      spawnSender :: Endpoint -> DiscordHandler ()
      spawnSender endpoint = void . async . forever $ do
        liftIO $ putStrLn "Awaiting (Discord) message dispatch"
        (message, targetChannelId) <- awaitMessageDispatched endpoint
        let body      = formatMessage message
            channelId = read . T.unpack . unChannelId $ targetChannelId
        Right _ <- restCall $ DR.CreateMessage channelId body
        return ()

