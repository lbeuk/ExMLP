-- Common dependencies from Data.ParserInternals are re-exported
module Parser (ParserHandle, ParserFn, makeHandle, ParsableSegment (..), ParseResult (..)) where

import Data.ByteString.Lazy (ByteString)
import Data.CaptureDict (CaptureDict)
import Data.ParserInternals.ParsableSegment (ParsableSegment (..))
import Data.ParserInternals.ParseResult (ParseResult (..))

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
       in -- This takes the result, and creates a new handle with the new state built in.
          (res, ParserHandle (makeHandleInner helper newSt))
