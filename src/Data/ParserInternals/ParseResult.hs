module Data.ParserInternals.ParseResult where
  
import Data.ParserInternals.ParsableSegment ( ParsableSegment )
import Data.Sequence (Seq)
import Data.CaptureDict (CaptureDict)

-- | Result of a parse 
data ParseResult = ParseResult CaptureDict (Seq ParsableSegment)

type ParseResults = Seq ParseResult