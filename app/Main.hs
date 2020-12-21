module Main where

import Config
import Control.Concurrent.Async
import Control.Monad
import Core
import Data.Maybe
import Data.Text (Text)
import GHC.Exts
import Optics
import Providers.API
import Providers.Telegram ()
import Providers.Discord ()
import System.Environment
import TextShow

main :: IO ()
main = do
  configFile <- fromMaybe "dyco-mirror.yml" <$> lookupEnv "DYCO_MIRROR_CONF"
  config@Config { name = botName, telegram = telegramConfig, discord = discordConfig, mirrors } <- readConfig configFile
  print config
  telegram <- spawnProviderEndpoint telegramConfig
  discord  <- spawnProviderEndpoint discordConfig
  runMirrorMap botName mirrors [
      (Telegram, (telegram, telegramConfig ^. #channels))
    , (Discord, (discord, discordConfig ^. #channels))
    ]

runMirrorMap :: Text -> [MirrorConfig] -> [(Provider, (Endpoint, [ChannelConfig]))] -> IO ()
runMirrorMap botName mirrors endpoints = do
  forConcurrently_ mirrorsBySource $ \(sourceProvider, mirrors) -> do
    case lookup sourceProvider endpoints of
      Nothing -> fail $ "Source provider " <> show sourceProvider <> " not configured"
      Just (sourceEndpoint, _) -> forever $ do
        putStrLn $ "Listening on " <> show sourceProvider <> " events..."
        message <- awaitMessageReceived sourceEndpoint
        printT message
        printT $ message ^. #user % #name
        unless (message ^. #user % #name == botName) $ do -- skip messages from the bot itself
          let matchingMirrors = filter (\MirrorConfig{source = ChannelRef{channelName}} -> channelName == message ^. #channel % #name) mirrors
          putStrLn $ "Matched mirrors: " <> show matchingMirrors
          forM_ matchingMirrors $ \MirrorConfig{target = ChannelRef{provider = targetProvider, channelName = targetChannelName}} -> do
            case lookup targetProvider endpoints of
              Nothing -> fail $ mconcat [ "Target provider ", show targetProvider, " not configured"]
              Just (targetEndpoint, channels) ->
                case channelIdByName targetChannelName channels of
                  Nothing -> fail $ mconcat ["Channel: ", show targetChannelName, " not found in provider", show targetProvider]
                  Just targetChannelId -> do
                    putStrLn "PUBLISHING MESSAGE"
                    publishMessage targetEndpoint message (ChannelId targetChannelId)
  where
    mirrorsBySource =
      [ (the sourceProvider, mirror)
      | mirror@MirrorConfig { source = ChannelRef { provider = sourceProvider } } <- mirrors
      , then group by sourceProvider using groupWith
      ]
