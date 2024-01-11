
module Data.ParserInternals.LinkedData where
import Data.CaptureDict (CaptureDict)
import Data.Sequence (Seq)
import Data.ParserInternals.ParsableSegment (ParsableSegment)

data LinkedCaptureDict = LinkedDict {
  sourceSegments :: Seq LinkedParsableSegment,
  dict :: CaptureDict
}

data LinkedParsableSegment = LinkedParsableSegment {
  sourceObject :: Seq LinkedCaptureDict,
  segment :: ParsableSegment
}

data LinkedParseResult = LinkedParseResult {
  parsed :: Seq LinkedCaptureDict,
  forwardedData :: Seq LinkedParsableSegment
}