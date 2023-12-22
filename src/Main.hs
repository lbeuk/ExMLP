import Data.List (intercalate)
import Data.Map (Map, foldlWithKey, fromList)
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Text.Printf (printf)

{- | IP Address Data Structure

__Examples:__

@
IPv6_Addr 0x0000 0x1111 0x2222 0x3333 0x4444 0x5555 0x6666 0x7777
IPv4_Addr 192 168 1 1
@
-}
data IPAddr
  = IPv4_Addr Word8 Word8 Word8 Word8
-- ^ IPv4 address constructor, made up of four 8-bit words
  | IPv6_Addr Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16
-- ^ IPv6 address constructor, made up of 8 16-biy words

instance Show IPAddr where
  show :: IPAddr -> String
  show (IPv4_Addr w1 w2 w3 w4) =
    intercalate "." $ map show [w1, w2, w3, w4]
  show (IPv6_Addr w1 w2 w3 w4 w5 w6 w7 w8) =
    intercalate ":" $ map (printf "%04x") [w1, w2, w3, w4, w5, w6, w7, w8]

{- | Parsed PCAP Field Data Structure

-}
data ParsedPcapMapData
  = ParsedPcapSubMap ParsedPcapMap
-- ^ Nested map
  | IPAddr IPAddr
-- ^ IP address
  | Str String
-- ^ Plain-test string


instance Show ParsedPcapMapData where
  show :: ParsedPcapMapData -> String
  show (Str s) = s
  show (IPAddr a) = show a
  show (ParsedPcapSubMap m) = show m

{- | Parsed PCAP Data Structure

-}
newtype ParsedPcapMap = ParsedPcapMap (Map String ParsedPcapMapData)

instance Show ParsedPcapMap where
  show :: ParsedPcapMap -> String
  show = showPcapLevel 0
    where
      showPcapLevel :: Int -> ParsedPcapMap -> String
      showPcapLevel lev (ParsedPcapMap m) =
        foldlWithKey (\prev key val -> prev ++ showData lev lev key val) "" m

      showData :: Int -> Int -> String -> ParsedPcapMapData -> String
      -- Case: Recursive: Show sub-map
      showData lev 0 key (ParsedPcapSubMap m) = key ++ ":\n" ++ showPcapLevel (lev + 1) m
      -- Case: Non-recursive, show data
      showData lev 0 key dat = key ++ ": " ++ show dat ++ "\n"
      -- Case: Needs more indentation
      showData lev left key dat = "\t" ++ showData lev (left - 1) key dat

main :: IO ()
main = do
  let x = IPv6_Addr 0x0000 0x1111 0x0000 0x3333 0x4444 0x5555 0x6666 0x7777 :: IPAddr
  let y = IPv4_Addr 192 168 1 1
  let a = ParsedPcapMap (fromList [("1", IPAddr x), ("2", IPAddr y), ("trashy", Str "Hello world")])
  let z = ParsedPcapMap (fromList [("1", IPAddr x), ("2", IPAddr y), ("submap", ParsedPcapSubMap a)])

  print z
