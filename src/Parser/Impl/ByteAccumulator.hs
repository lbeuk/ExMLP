module Parser.Impl.ByteAccumulator where

import Data.ByteString.Lazy as BS (length)
import Data.Int (Int64)
import Data.Sequence (fromList)
import Data.ParserInternals.LinkedData (LinkedParsableSegment(LinkedParsableSegment, segment), LinkedParseResult (LinkedParseResult))
import Data.ParserInternals.ParserHandle ( ParserHandle )
import Data.ParserInternals.ParsableSegment
    ( ParsableSegment(bytes) )
import Data.ParserInternals.ParseResult
    ( ParseResult(ParseResult) )
import Data.ParserInternals.LinkageHelper ( LinkageHelper(..) )
import Parser.LinkageAssisted (LinkageAssistedParserFn, makeHandle)

-- | Basic example parser that accumulates the number of bytes read
byteAccumulatorParser :: ParserHandle
byteAccumulatorParser = makeHandle byteAccumulatorParserFn 0

byteAccumulatorParserFn :: LinkageAssistedParserFn Int64
byteAccumulatorParserFn st segment = 
  (fromList [], Just ( st + BS.length (bytes segment)))