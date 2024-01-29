module Data.ParserInternals.LinkedData where

import Data.ParserInternals.ParsableSegment (ParsableSegment)
import Data.ParserInternals.ParseResult
import Data.Sequence (Seq, empty, foldlWithIndex, mapWithIndex, (><), (|>))

data LinkedOutput out = LinkedOutput
  { sourceSegments :: Seq (LinkedParsableSegment out),
    dat :: out
  }

data LinkedParsableSegment out = LinkedParsableSegment
  { sourceObject :: LinkedOutput out,
    segment :: ParsableSegment
  }

data LinkedParseResult out = LinkedParseResult (LinkedOutput out) (Seq (LinkedParsableSegment out))

type LinkedParseResults out = Seq (LinkedParseResult out)

linkParseResult :: ParseResult out -> Seq (LinkedParsableSegment out) -> LinkedParseResult out
linkParseResult (ParseResult unlinkedDict unlinkedSegments) srcSegments =
  let linkedDict =
        LinkedOutput
          { sourceSegments = srcSegments,
            dat = unlinkedDict
          }
      linkedSegments =
        mapWithIndex
          ( \i unlinkedSegment ->
              LinkedParsableSegment
                { sourceObject = linkedDict,
                  segment = unlinkedSegment
                }
          )
          unlinkedSegments
   in LinkedParseResult linkedDict linkedSegments

forwardedData :: LinkedParseResults out -> Seq (LinkedParsableSegment out)
forwardedData = foldlWithIndex (\curSeq i (LinkedParseResult _ newSeq) -> curSeq >< newSeq) empty

parsedData :: LinkedParseResults out -> Seq (LinkedOutput out)
parsedData = foldlWithIndex (\curSeq i (LinkedParseResult newDict _) -> curSeq |> newDict) empty