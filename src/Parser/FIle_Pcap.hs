module Parser.File_Pcap where

import Conversion (Conversion (convert))
import Data.Binary.Get
  ( Get,
    getInt32le,
    getWord16le,
    getWord32le,
    runGet,
  )
import Data.ByteString.Lazy as BS
import Data.Int (Int32, Int64)
import Data.Map (fromList)
import Data.Word (Word16, Word32)
import Numeric (showHex)
import Data.CaptureDict (CaptureDict (CaptureDict), CaptureDictData (Str))
import qualified Data.IntMap as IM
-- import Protocols (NetProtocol (NetProtocolL2, NoProtocol), L2Protocol (Ethernet))

-- Maps network field to corresponding protocol
rootProtocol :: Word32 -> NetProtocol
rootProtocol 1 = NetProtocolL2 Ethernet
rootProtocol _ = NoProtocol


-- | Header block of PCAP file.
data PcapHeaderBlock = PcapHeaderBlock
  { magicNumber :: Word32,
    versionMajor :: Word16,
    versionMinor :: Word16,
    thiszone :: Int32,
    sigfigs :: Word32,
    snaplen :: Word32,
    network :: Word32
  }
  deriving (Show)

getPcapHeaderBlock :: Get PcapHeaderBlock
getPcapHeaderBlock =
  PcapHeaderBlock
    <$> getWord32le
    <*> getWord16le
    <*> getWord16le
    <*> getInt32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

-- | Converts a PcapHeaderBlock into a CaptureDict
renderPcapHeaderBlock :: PcapHeaderBlock -> CaptureDict
renderPcapHeaderBlock block =
  CaptureDict
    ( fromList
        [ ("Magic Number", Str (showHex (magicNumber block) "")),
          ("Major Version", Str (show (versionMajor block))),
          ("Minor Version", Str (show (versionMinor block))),
          ("Time Zone", Str (show (thiszone block))),
          ("Sig Figs", Str (show (sigfigs block))),
          ("Snap Length", Str (show (snaplen block))),
          ("Network", Str (show (network block)))
        ]
    )

-- | Header block of PCAP record
data PcapRecordHeaderBlock = PcapRecordHeaderBlock
  { tsSec :: Word32,
    tsUSec :: Word32,
    inclLen :: Word32,
    origLen :: Word32
  }
  deriving (Show)

getPcapRecordHeaderBlock :: Get PcapRecordHeaderBlock
getPcapRecordHeaderBlock =
  PcapRecordHeaderBlock
    <$> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

-- | Helper for extractPcapRecords that handles
--  record block extraction.
extractPcapRecordBlock :: Int64 -> PcapHeaderBlock -> [RawPacket] -> ByteString -> Maybe [RawPacket]
extractPcapRecordBlock len header prev bs
  | BS.length bs >= len =
      let (packetBody, rest) = BS.splitAt len bs
          rawPacket = RawPacket (packetBody, rootProtocol (network header))
          newPacketList = prev ++ [rawPacket]
       in extractPcapRecords header newPacketList rest
  | otherwise = Nothing

-- | Extracts a list of pcap records from the bytes of a
--  pcap file, used after pcap file header is extracted.
extractPcapRecords :: PcapHeaderBlock -> [RawPacket] -> BS.ByteString -> Maybe [RawPacket]
extractPcapRecords header prev bs
  | BS.length bs >= 16 =
      let (recordHeaderBytes, rest) = BS.splitAt 16 bs
          recordHeader = runGet getPcapRecordHeaderBlock recordHeaderBytes
          recordBlockLen :: Int64 = convert (inclLen recordHeader)
       in extractPcapRecordBlock recordBlockLen header prev rest
  | BS.length bs == 0 = Just prev
  | otherwise = Nothing

-- | File parser for pcap files
pcapParser :: FileParser
pcapParser bs =
  let (headerBytes, pcapBody) = BS.splitAt 24 bs
      header = runGet getPcapHeaderBlock headerBytes
      headerMap = renderPcapHeaderBlock header
      prot = network header
      records = extractPcapRecords header [] pcapBody
   in case records of
        Just r -> Just (headerMap, r)
        Nothing -> Nothing