module Core where

import Data.Text
import Optics.TH
import Optics.Operators

import TextShow
import TextShow.Generic
import GHC.Generics

newtype UserRef = UserRef { name :: Text }
  deriving newtype TextShow

data Message = Message
  { user :: UserRef
  , channel :: Channel
  , content :: Text
  }
  deriving Generic
  deriving TextShow via FromGeneric Message

formatMessage :: Message -> Text
formatMessage Message {..} = mconcat ["[#", channel ^. #name, "]: ", user ^. #name, ": ", content]

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving newtype TextShow

-- newtype ChannelName = ChannelName { unChannelName :: Text }
--   deriving TextShow

data Channel = Channel { id :: ChannelId, name :: Text }
  deriving Generic
  deriving TextShow via FromGeneric Channel

makeFieldLabelsWith noPrefixFieldLabels ''UserRef
makeFieldLabelsWith noPrefixFieldLabels ''Message
makeFieldLabelsWith noPrefixFieldLabels ''Channel
