module Data.ExMLP.ParsableSegment where

import Data.ByteString.Lazy (ByteString)
import Data.ExMLP.Signature (SignatureStack)
import Data.Sequence (Seq)

-- | A binary segment with a signature tagged to it
data ParsableSegment = ParsableSegment
  { signature :: SignatureStack,
    bytes :: ByteString
  }