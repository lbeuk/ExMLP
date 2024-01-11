module Data.ParserInternals.Signature where

-- | Signature to identify a segment, used in identifying a parser to feed it to
data Signature
  = Ethernet
  | TCP

-- | List of signatures (for stacked protocols)
type SignatureStack = [Signature]