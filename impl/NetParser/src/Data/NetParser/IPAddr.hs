module Data.NetParser.IPAddr where

import Data.List (intercalate)
import Data.Word (Word16, Word8)
import Text.Printf (printf)
import Data.Int (Int8)
    
-- | IP Address Data Structure
--
-- __Examples:__
--
-- @
-- IPv6_Addr 0x0000 0x1111 0x2222 0x3333 0x4444 0x5555 0x6666 0x7777
-- IPv4_Addr 192 168 1 1
-- @
data IPAddr
  = -- | IPv4 address constructor, made up of four 8-bit words
    IPv4_Addr Word8 Word8 Word8 Word8
  | -- | IPv6 address constructor, made up of 8 16-biy words
    IPv6_Addr Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16

instance Show IPAddr where
  show (IPv4_Addr w1 w2 w3 w4) =
    intercalate "." (map show [w1, w2, w3, w4])
  show (IPv6_Addr w1 w2 w3 w4 w5 w6 w7 w8) =
    intercalate ":" (map (printf "%04x") [w1, w2, w3, w4, w5, w6, w7, w8])

data CIDRAddr = CIDRAddr IPAddr Word8

instance Show CIDRAddr where
  show (CIDRAddr ip sn) = show ip ++ "/" ++ show sn