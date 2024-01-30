module Data.ExMLP.ParserHandle where
import Data.ExMLP.LinkedData (LinkedParsableSegment, LinkedParseResults)

-- | Handle for storing a parser
newtype ParserHandle out = ParserHandle (LinkedParsableSegment out -> (LinkedParseResults out, Maybe (ParserHandle out)))