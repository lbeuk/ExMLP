module Data.ParserInternals.ParsableSegment where
import Data.ParserInternals.Signature (SignatureStack)
import Data.ByteString.Lazy (ByteString)

-- | A binary segment with a signature tagged to it
data ParsableSegment = ParsableSegment {
  signature :: SignatureStack, 
  bytes :: ByteString
}