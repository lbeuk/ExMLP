module Parser where
import Data.ByteString.Lazy
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
  parsedData :: [CaptureDict],
  forwardedData :: [ParsableSegment]
}

-- | Handle for storing a parser
newtype ParserHandle = ParserHandle (ParsableSegment -> (ParseResult, ParserHandle))

-- | Inner function that is implemented to construct parser
type ParserFn a = a -> ParsableSegment -> (ParseResult, a)

-- | Transforms a ParserFn with a default state value into a handle
makeHandle :: ParserFn a -> a -> ParserHandle
makeHandle helper def =
  ParserHandle (makeHandleInner helper def)
  where
    -- This inner function does the majority of the heavy lifting here.
    -- It takes in the helper function, the initial state, and a segment.
    -- The segment is not passed by makeHandle, so that the resulting
    -- function created takes in a segment, and this returns a tuple
    -- containing the result of the parse from that segment, as well as
    -- an updated handle with the new parser state.
    makeHandleInner :: ParserFn a -> a -> ParsableSegment -> (ParseResult, ParserHandle)
    makeHandleInner helper st segment =
      -- When the segment is recieved, this will call the helper of type ParserFn.
      let (res, newSt) = helper st segment
      -- This takes the result, and creates a new handle with the new state built in.
      in (res, ParserHandle (makeHandleInner helper newSt))
