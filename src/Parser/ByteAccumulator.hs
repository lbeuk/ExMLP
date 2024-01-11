module Parser.ByteAccumulator where

import Data.ByteString.Lazy as BS (length)
import Data.Int (Int64)
import Data.Sequence (fromList)
import Parser ( ParserFn, makeHandle )
import Data.ParserInternals.LinkedData (LinkedParsableSegment(LinkedParsableSegment, segment), LinkedParseResult (LinkedParseResult))
import Data.ParserInternals.ParserHandle ( ParserHandle )
import Data.ParserInternals.ParsableSegment
    ( ParsableSegment(bytes) )

-- | Basic example parser that accumulates the number of bytes read
byteAccumulatorParser :: ParserHandle
byteAccumulatorParser = makeHandle byteAccumulatorParserFn 0

byteAccumulatorParserFn :: ParserFn Int64
byteAccumulatorParserFn st (LinkedParsableSegment src segment) =
  (LinkedParseResult (fromList []) (fromList []), Just ( st + BS.length (bytes segment)))