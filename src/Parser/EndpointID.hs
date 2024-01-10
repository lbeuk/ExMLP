module Parser.EndpointID where

import Data.Signature (Signature)

data EndpointID
  = Ethernet

endpointParser :: EndpointID -> ParserFn