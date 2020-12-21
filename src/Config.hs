module Config where

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Text
import qualified Data.Yaml as Y
import GHC.Generics
import Optics.TH

data Config = Config
  { name :: Text,
    telegram :: TelegramConfig,
    discord :: DiscordConfig,
    mirrors :: [MirrorConfig]
  } deriving (Show, Generic, FromJSON)

data TelegramConfig = TelegramConfig
  { token :: Text
  , channels :: [ChannelConfig]
  } deriving (Show, Generic, FromJSON)

data DiscordConfig = DiscordConfig
  { token :: Text
  , channels :: [ChannelConfig]
  } deriving (Show, Generic, FromJSON)

data ChannelConfig = ChannelConfig {name :: Text, id :: Text} deriving (Show)

channelIdByName :: Text -> [ChannelConfig] -> Maybe Text
channelIdByName name channels = lookup name $ (\ChannelConfig {..} -> (name, id)) <$> channels

instance {-# OVERLAPPING #-} FromJSON [ChannelConfig] where
  parseJSON (Object v) = mapM parseChannel $ HM.toList v
    where
      parseChannel (channel, String id) = return $ ChannelConfig channel id
      parseChannel _                    = fail "Expected a key-value String pair"
  parseJSON _ = fail "Expected an object"

data MirrorConfig = MirrorConfig {source :: ChannelRef, target :: ChannelRef} deriving (Show, Generic, FromJSON, Eq)

data Provider = Telegram | Discord deriving (Show, Generic, FromJSON, Ord, Eq)

data ChannelRef = ChannelRef {provider :: Provider, channelName :: Text} deriving (Show, Eq)

instance FromJSON ChannelRef where
  parseJSON (String (splitOn "/" -> [provider, channelName])) =
    ChannelRef <$> parseProvider (toLower provider) <*> return channelName
    where
      parseProvider "telegram" = return Telegram
      parseProvider "discord"  = return Discord
      parseProvider _          = fail "Expected either <telegram> or <discord> chat provider"
  parseJSON _ = fail "Expected <provider/channel>"

readConfig :: String -> IO Config
readConfig = BS.readFile >=> Y.decodeThrow

makeFieldLabelsWith noPrefixFieldLabels ''Config
makeFieldLabelsWith noPrefixFieldLabels ''TelegramConfig
makeFieldLabelsWith noPrefixFieldLabels ''DiscordConfig
makeFieldLabelsWith noPrefixFieldLabels ''ChannelConfig
makeFieldLabelsWith noPrefixFieldLabels ''MirrorConfig
makeFieldLabelsWith noPrefixFieldLabels ''ChannelRef
