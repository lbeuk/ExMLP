module Parser.ByteAccumulator where

import Data.ByteString.Lazy as BS (length)
import Data.Int (Int64)
import Parser
  ( ParsableSegment (bytes),
    ParseResult (ParseResult),
    ParserHandle (..), ParserFn, makeHandle,
  )

byteAccumulatorParser :: ParserHandle
byteAccumulatorParser = makeHandle byteAccumulatorParserFn 0

byteAccumulatorParserFn :: ParserFn Int64
byteAccumulatorParserFn st segment =
  (ParseResult [] [], st + BS.length (bytes segment))