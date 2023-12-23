module ProtocolParser where

import Data.ByteString.Lazy as BS
import Data.CaptureDict (CaptureDict)
import Protocols (NetProtocol)

-- Type representing a raw packet's data
newtype RawPacket = RawPacket (ByteString, NetProtocol)

-- | Type representing a list of ByteStrings, each corresponding to a packet
type RawPacketList = [RawPacket]

-- | Type representing a function to handle reading in a file structure
type FileParser = BS.ByteString -> Maybe (CaptureDict, RawPacketList)