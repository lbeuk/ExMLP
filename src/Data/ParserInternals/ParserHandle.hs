module Data.ParserInternals.ParserHandle where
import Data.ParserInternals.LinkedData (LinkedParsableSegment, LinkedParseResults)

-- | Handle for storing a parser
newtype ParserHandle = ParserHandle (LinkedParsableSegment -> (LinkedParseResults, Maybe ParserHandle))