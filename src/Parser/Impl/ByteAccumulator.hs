module Parser.Impl.ByteAccumulator where

import Data.ByteString.Lazy as BS (length)
import Data.Int (Int64)
import Data.Sequence (fromList)
import Parser ( ParserFn, makeHandle, LinkageAssistedParserFn )
import Data.ParserInternals.LinkedData (LinkedParsableSegment(LinkedParsableSegment, segment), LinkedParseResult (LinkedParseResult))
import Data.ParserInternals.ParserHandle ( ParserHandle )
import Data.ParserInternals.ParsableSegment
    ( ParsableSegment(bytes) )
import Data.ParserInternals.ParseResult
    ( ParseResult(ParseResult) )
import Data.ParserInternals.LinkageHelper ( LinkageHelper(..) )

-- | Basic example parser that accumulates the number of bytes read
byteAccumulatorParser :: ParserHandle
byteAccumulatorParser = makeHandle byteAccumulatorParserFn 0


byteAccumulatorParser' :: LinkageAssistedParserFn Int64
byteAccumulatorParser' st segment = 
  (ParseResult (fromList []) (fromList []), Just ( st + BS.length (bytes segment)))

byteAccumulatorParserFn :: ParserFn (LinkageHelper Int64)
byteAccumulatorParserFn oldOuterSt linkedSeg =
  let (res, newInnerSt) = byteAccumulatorParser' (dat oldOuterSt) (segment linkedSeg)
      newLinkage = linkedSeg <| linked oldOuterSt
      newOuterSt = case newInnerSt of
        Nothing -> Nothing
        Just newInnerSt' -> LinkageHelper {
          linked = newLinkage,
          dat = newInnerSt'
        }