module Parser.Signature where

import Data.Binary (Word16)
import Data.MACAddr (MACAddr)

data Signature
  = -- (OSI Level 1) Physical
    LibPCAP
      { linkType :: Int
      }
  | -- (OSI Level 2) Data link
    IEEE_802_3
      { src :: MACAddr,
        dst :: MACAddr,
        length :: Word16
      }
  | Eth2
      { src :: MACAddr,
        dst :: MACAddr,
        ethtype :: Word16
      }
  | -- (OSI Level 3) Network
    IPv4
  | IPv6
  | -- (OSI Level 4) Transport
    UDP
  | TCP
  | -- (OSI Level 5) Application
    HTTP
  | -- Document / Data
    Text
  | XML
  | JSON
  deriving (Show)