
module Data.ParserInternals.LinkedData where
import Data.CaptureDict (CaptureDict)
import Data.Sequence (Seq, mapWithIndex, foldlWithIndex, empty, (><), (|>))
import Data.ParserInternals.ParsableSegment (ParsableSegment)
import Data.ParserInternals.ParseResult

data LinkedCaptureDict = LinkedCaptureDict {
  sourceSegments :: Seq LinkedParsableSegment,
  dict :: CaptureDict
}

data LinkedParsableSegment = LinkedParsableSegment {
  sourceObject :: LinkedCaptureDict,
  segment :: ParsableSegment
}

data LinkedParseResult = LinkedParseResult LinkedCaptureDict (Seq LinkedParsableSegment)

type LinkedParseResults = Seq LinkedParseResult

linkParseResult :: ParseResult -> Seq LinkedParsableSegment -> LinkedParseResult
linkParseResult (ParseResult unlinkedDict unlinkedSegments) srcSegments =
  let linkedDict = LinkedCaptureDict {
        sourceSegments = srcSegments,
        dict = unlinkedDict
      }
      linkedSegments = mapWithIndex (\i unlinkedSegment -> LinkedParsableSegment {
        sourceObject = linkedDict,
        segment = unlinkedSegment
      }) unlinkedSegments
    in LinkedParseResult linkedDict linkedSegments

forwardedData :: LinkedParseResults -> Seq LinkedParsableSegment
forwardedData = foldlWithIndex (\curSeq i (LinkedParseResult _ newSeq) -> curSeq >< newSeq) empty

parsedData :: LinkedParseResults -> Seq LinkedCaptureDict
parsedData = foldlWithIndex (\curSeq i (LinkedParseResult newDict _) -> curSeq |> newDict) empty