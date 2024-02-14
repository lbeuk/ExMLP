module Data.CaptureDict where

import Data.Map (Map, foldlWithKey, toList)
import Data.List (intercalate)
import Data.NetParser.IPAddr

-- | Parsed Capture Field Data Structure
data CaptureDictData
  = SubDict CaptureDict
  | -- \^ Nested map
    List [CaptureDictData]
  | -- \^ List of data
    IPAddr IPAddr
  | -- \^ IP address
    Str String
-- ^ Plain-test string
  deriving (Show)


-- | Parsed  Data Structure
newtype CaptureDict = CaptureDict (Map String CaptureDictData)

-- EVERYTHING BELOW THIS POINT IS YAML PRINTING LOGIC

instance Show CaptureDict where
  show d = showDict d 0

-- | Show helper to calculate preceeding whitespace
showIndentation :: Int -> String
showIndentation 0 = ""
showIndentation n = "  " ++ showIndentation (n - 1)

-- | Show helper to show a dict, but without preceeding indentation
showDictNoIndentFirst :: CaptureDict -> Int -> String
showDictNoIndentFirst (CaptureDict d) indent = intercalate
  ("\n" ++ showIndentation indent)
  (map (\(key, val) -> key ++ ": " ++ showDictValue val indent) (toList d))

-- | Show helper to show a dict
showDict :: CaptureDict -> Int -> String
showDict dict indent = showIndentation indent ++ showDictNoIndentFirst dict indent

-- | Show helper to show a list inside a dict, but without preceeding indentation
showDictListNoIndentFirst :: [CaptureDictData] -> Int -> String
showDictListNoIndentFirst list indent = intercalate
  ("\n" ++ showIndentation indent)
  (map (\val -> "- " ++ showDictListValue val indent) list)

-- | Show helper to show a list inside a dict
showDictList :: [CaptureDictData] -> Int -> String
showDictList list indent = showIndentation indent ++ showDictListNoIndentFirst list indent

{- | Show helper to deal with showing values in a list.

This is necessary due to some weird indentation rules with YAML,
primarily that there are no newlines before a dict or sublist when
a list item.
-}
showDictListValue :: CaptureDictData -> Int -> String
showDictListValue (SubDict s) indent = showDictNoIndentFirst s (indent + 1)
showDictListValue (List l) indent = showDictListNoIndentFirst l (indent + 1)
showDictListValue val indent = showDictValue val indent

{- | Show helper to show a CaptureDictValue

This is the primary handler for handling different value types' formatting.
-}
showDictValue :: CaptureDictData -> Int -> String
showDictValue (SubDict d) indent = "\n" ++ showDict d (indent + 1)
showDictValue (List l) indent = "\n" ++ showDictList l (indent + 1)
showDictValue (IPAddr i) _ = show i
showDictValue (Str s) _ = s