module Providers.Telegram where

import Config
import Core
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader.Class
import Data.Text as T
import qualified Data.Text.IO as TIO
import Providers.API
import Servant.Client hiding (Response)
import Telegram.Bot.API hiding (Message)
import qualified Telegram.Bot.API as TBA
import Telegram.Bot.Simple.BotApp.Internal ( startPolling )
import TextShow
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (toStrict)

bot :: Endpoint -> ClientM ()
bot endpoint = do
  fork_ $ startPolling onUpdate
  publishLoop
  where
    fork_ m = do
      env <- ask
      let run = do
            Right result <- runClientM m env
            return result
      liftIO $ async run

    onUpdate Update {..} = liftIO $ maybe (return ()) (onMessageReceived endpoint) msg
      where
        msg = do
          message  <- updateMessage
          userName <- userUsername =<< messageFrom message
          let channel   = messageChat message
          let channelId = chatId channel
          channelName <- chatTitle channel
          content     <- TBA.messageText message
          return $ Message (UserRef userName)
                            (Channel (ChannelId . toStrict . encodeToLazyText $ channelId) channelName)
                            content

    publishLoop = forever $ do
      liftIO $ putStrLn "Awaiting (Telegram) message dispatch"
      (message, targetChannelId) <- liftIO $ awaitMessageDispatched endpoint
      let body = formatMessage message
      liftIO . TIO.putStrLn $ mconcat ["Sending ", body, " to ", showt targetChannelId]
      sendTo (ChatId . read . T.unpack . unChannelId $ targetChannelId) body

sendTo :: ChatId -> Text -> ClientM (Response TBA.Message)
sendTo chatId msgText = sendMessage SendMessageRequest { sendMessageChatId                = SomeChatId chatId
                                                       , sendMessageText                  = msgText
                                                       , sendMessageParseMode             = Nothing
                                                       , sendMessageDisableWebPagePreview = Nothing
                                                       , sendMessageDisableNotification   = Nothing
                                                       , sendMessageReplyToMessageId      = Nothing
                                                       , sendMessageReplyMarkup           = Nothing
                                                       }

instance ProviderEndpoint TelegramConfig where
  spawnProviderEndpoint TelegramConfig {..} =
    withNewEndpoint $ \endpoint -> void . async $ defaultRunBot (Token token) (bot endpoint)
