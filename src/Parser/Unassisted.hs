module Parser.Unassisted where
import qualified Data.ParserInternals.LinkedData as Linked
import Data.ParserInternals.ParserHandle ( ParserHandle(..) )

-- | Inner function that is implemented to construct parser
type UnassistedParserFn a = a -> Linked.LinkedParsableSegment -> (Linked.LinkedParseResults, Maybe a)

-- | Transforms a ParserFn with a default state value into a handle
makeHandle :: UnassistedParserFn a -> a -> ParserHandle
makeHandle helper def =
  ParserHandle (makeHandleInner helper def)
  where
    -- This inner function does the majority of the heavy lifting here.
    -- It takes in the helper function, the initial state, and a segment.
    -- The segment is not passed by makeHandle, so that the resulting
    -- function created takes in a segment, and this returns a tuple
    -- containing the result of the parse from that segment, as well as
    -- an updated handle with the new parser state.
    makeHandleInner :: UnassistedParserFn a -> a -> Linked.LinkedParsableSegment -> (Linked.LinkedParseResults, Maybe ParserHandle)
    makeHandleInner helper st segment =
      -- When the segment is recieved, this will call the helper of type ParserFn.
      let (res, newSt) = helper st segment
          newHandle = case newSt of
            Nothing -> Nothing
            Just newSt' -> Just (ParserHandle (makeHandleInner helper newSt'))
       in (res, newHandle)