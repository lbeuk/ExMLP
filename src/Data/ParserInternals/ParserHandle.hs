module Data.ParserInternals.ParserHandle where
import Data.ParserInternals.LinkedData (LinkedParsableSegment, LinkedParseResults)

-- | Handle for storing a parser
newtype ParserHandle out = ParserHandle (LinkedParsableSegment out -> (LinkedParseResults out, Maybe (ParserHandle out)))