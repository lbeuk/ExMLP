module Parser where

import Data.ByteString.Lazy (ByteString)

type SignatureFilter = [Signature] -> Maybe EndpointID

-- | Passes a signature past a list of filters until a endpoint is found
endpointIDFromSignature :: [Signature] -> [SignatureFilter] -> Maybe EndpointID
endpointIDFromSignature sig [] -> Nothing
endpointIDFromSignature sig (filter:t) -> case (filter sig) of
  Just eid -> Just eid
  Nothing -> endpointIDFromSignature sig t

endpointMapLol :: 


type ParseResult
  = Success Signature
  | Incomplete
  | Fail_Reset

type ParserFn = ByteString -> ParseResult

type EndpointMap = Map EndpointID [ByteString]

data ParserState = ParserState {
  endpoints :: EndpointMap,
  filters :: [SignatureFilter]
}

process :: ParserState -> ByteString -> ParserState
process state bs = case (endpointIDFromSignature  () )
  