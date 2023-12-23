module FileParser where

import Data.ByteString.Lazy as BS
import Data.CaptureDict (CaptureDict)
import ProtocolParser (RawPacket)

-- | Type representing a function to handle reading in a file structure
type FileParser = BS.ByteString -> Maybe (CaptureDict, [RawPacket])


