module Data.ExMLP.ParseManager where

import Data.Map
import Data.Map as M (Map, delete, insert, lookup)
import qualified Data.ExMLP.LinkedData as Linked
  ( LinkedOutput,
    LinkedParsableSegment,
    forwardedData,
    parsedData,
  )
import Data.ExMLP.ParserHandle ( ParserHandle(..) )
import Data.ExMLP.Reciever
    ( RecieverID, Reciever(def, rid) )
import Data.Sequence (Seq, (><))

type RecieverMap out = Map RecieverID (ParserHandle out)

-- | Function that attempts to identify a segment
type RecieverIdentifier out = Linked.LinkedParsableSegment out -> Maybe (Reciever out)

-- | Checks a segment against a chain of identifers and attempts to find a reciever
identifyReciever :: Linked.LinkedParsableSegment out -> [RecieverIdentifier out] -> Maybe (Reciever out)
identifyReciever _ [] = Nothing
identifyReciever seg (identifier : t) = case identifier seg of
  Nothing -> identifyReciever seg t
  Just id -> Just id

-- | Manager for the parsing process
data ParseManager out = ParseManager
  { -- | Map of recievers that are currently open
    activeRecievers :: RecieverMap out,
    -- | List of identifier functions to identify inactive recievers
    recieverIdentifiers :: [RecieverIdentifier out],
    -- | ParsableSegments waiting to be processed
    parseQueue :: Seq (Linked.LinkedParsableSegment out),
    -- | Parsed data
    parsedData :: Seq (Linked.LinkedOutput out)
  }

-- | Parse a ParsableSegment and update parser
parse :: Linked.LinkedParsableSegment out -> ParseManager out -> ParseManager out
parse segment parser =
  let identifier = identifyReciever segment (recieverIdentifiers parser)
   in parseWithIdentifer identifier segment parser
  where
    -- | Create new parser
    parseWithIdentifer :: Maybe (Reciever out) -> Linked.LinkedParsableSegment out -> ParseManager out -> ParseManager out
    parseWithIdentifer Nothing _ parser = parser
    parseWithIdentifer (Just reciever) segment parser =
      let oldRecieverMap = activeRecievers parser
          ParserHandle handle = getHandle reciever parser -- Gets handler based on current parser state
          (result, newHandle) = handle segment -- Handles the segment, returning a tuple of
       in ParseManager
            { activeRecievers = updateRecievers newHandle (rid reciever) oldRecieverMap, -- Creates the new reciever map based on the resultcieverMap
              recieverIdentifiers = recieverIdentifiers parser, -- Copies reciever identifiers from old parser
              parseQueue = Linked.forwardedData result >< parseQueue parser, -- Prepends the new ParsableSegments to the current list
              parsedData = Linked.parsedData result >< parsedData parser
            }

    -- | Assists in getting the handle of a reciever, returning any active handle before returning a fresh one
    getHandle :: Reciever out -> ParseManager out -> ParserHandle out
    getHandle reciever parser =
      case M.lookup (rid reciever) (activeRecievers parser) of
        Nothing -> def reciever -- Default handle for the reciever
        Just handle -> handle -- Existing active handle

    -- | Updates the reciever map with new state
    updateRecievers :: Maybe (ParserHandle out) -> RecieverID -> RecieverMap out -> RecieverMap out
    updateRecievers Nothing rid oldMap = M.delete rid oldMap
    updateRecievers (Just handle) rid oldMap = M.insert rid handle oldMap