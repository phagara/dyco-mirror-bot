module Providers.Discord where

import Core
import Config
import Control.Concurrent.Async
import Control.Monad
import Data.Text as T
import Discord
import qualified Discord.Types as DT
import qualified Discord.Requests as DR
import Providers.API

instance ProviderEndpoint DiscordConfig where
  spawnProviderEndpoint DiscordConfig {..} = withNewEndpoint $ \endpoint -> void . async . runDiscord $ def
    { discordToken   = token
    , discordOnStart = spawnSender endpoint
    , discordOnEvent = eventHandler endpoint
    }
    where
      eventHandler :: Endpoint -> DiscordHandle -> DT.Event -> IO ()
      eventHandler endpoint handle (DT.MessageCreate m) = do
        putStrLn "Handling Discord event"
        Right channelName <- fmap DT.channelName <$> getChannel cid
        let channel = Channel channelId channelName
        putStr "Forwarding message"
        when (content /= "") $ onMessageReceived endpoint Message { .. } -- ignore images for now
        where
          user       = UserRef . DT.userName . DT.messageAuthor $ m
          cid        = DT.messageChannel m
          channelId  = ChannelId $ T.pack . show $ cid
          content    = DT.messageText m
          getChannel = restCall handle . DR.GetChannel
      eventHandler _ _ _ = return ()

      spawnSender :: Endpoint -> DiscordHandle -> IO ()
      spawnSender endpoint handle = void . async . forever $ do
        putStrLn "Awaiting (Discord) message dispatch"
        (message, targetChannelId) <- awaitMessageDispatched endpoint
        let body      = formatMessage message
            channelId = read . T.unpack . unChannelId $ targetChannelId
        Right _ <- restCall handle $ DR.CreateMessage channelId body
        return ()

