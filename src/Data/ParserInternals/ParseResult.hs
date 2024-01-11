module Data.ParserInternals.ParseResult where
import Data.CaptureDict ( CaptureDict )
import Data.ParserInternals.ParsableSegment ( ParsableSegment )

-- | Result of a parse 
data ParseResult = ParseResult {
  parsedData :: [CaptureDict],
  forwardedData :: [ParsableSegment]
}