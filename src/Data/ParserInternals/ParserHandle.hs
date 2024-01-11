module Data.ParserInternals.ParserHandle where
import Data.ParserInternals.LinkedData (LinkedParsableSegment, LinkedParseResult)

-- | Handle for storing a parser
newtype ParserHandle = ParserHandle (LinkedParsableSegment -> (LinkedParseResult, Maybe ParserHandle))