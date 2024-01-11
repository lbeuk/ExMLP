module Parser where
import Data.ByteString.Lazy
import Control.Monad.State (State)
import Data.CaptureDict ( CaptureDict )

-- | Signature to identify a segment, used in identifying a parser to feed it to
data Signature
  = Ethernet
  | TCP

-- | List of signatures (for stacked protocols)
type SignatureStack = [Signature]

-- | A binary segment with a signature tagged to it
data ParsableSegment = ParsableSegment {
  signature :: SignatureStack, 
  bytes :: ByteString
}

-- | Result of a parse 
data ParseResult = ParseResult {
  parsedData :: Maybe CaptureDict,
  forwardedData :: Maybe ParsableSegment
}

type ParserState = State ParserFn

newtype ParserFn = ParserFn (ByteString -> ParserState ParseResult)