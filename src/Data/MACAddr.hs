module Data.MACAddr where

import Data.Binary (Word8)
import Data.List (intercalate)
import Text.Printf (printf)

-- | MAC Address Data Structures
data MACAddr = MACAddr Word8 Word8 Word8 Word8 Word8 Word8

instance Show MACAddr where
  show (MACAddr w1 w2 w3 w4 w5 w6) =
    "[MAC]" ++ intercalate ":" (map (printf "%02x") [w1, w2, w3, w4, w5, w6])