module Data.ParserInternals.ParseManager where

import Data.Map
import Data.Map as M (Map, delete, insert, lookup)
import qualified Data.ParserInternals.LinkedData as Linked
  ( LinkedCaptureDict,
    LinkedParsableSegment,
    forwardedData,
    parsedData,
  )
import Data.ParserInternals.ParserHandle ( ParserHandle(..) )
import Data.ParserInternals.Reciever
    ( RecieverID, Reciever(def, rid) )
import Data.Sequence (Seq, (><))

type RecieverMap = Map RecieverID ParserHandle

-- | Function that attempts to identify a segment
type RecieverIdentifier = Linked.LinkedParsableSegment -> Maybe Reciever

-- | Checks a segment against a chain of identifers and attempts to find a reciever
identifyReciever :: Linked.LinkedParsableSegment -> [RecieverIdentifier] -> Maybe Reciever
identifyReciever _ [] = Nothing
identifyReciever seg (identifier : t) = case identifier seg of
  Nothing -> identifyReciever seg t
  Just id -> Just id

-- | Manager for the parsing process
data ParseManager = ParseManager
  { -- | Map of recievers that are currently open
    activeRecievers :: RecieverMap,
    -- | List of identifier functions to identify inactive recievers
    recieverIdentifiers :: [RecieverIdentifier],
    -- | ParsableSegments waiting to be processed
    parseQueue :: Seq Linked.LinkedParsableSegment,
    -- | Parsed data
    parsedData :: Seq Linked.LinkedCaptureDict
  }

-- | Parse a ParsableSegment and update parser
parse :: Linked.LinkedParsableSegment -> ParseManager -> ParseManager
parse segment parser =
  let identifier = identifyReciever segment (recieverIdentifiers parser)
   in parseWithIdentifer identifier segment parser
  where
    -- \| Create new parser
    parseWithIdentifer :: Maybe Reciever -> Linked.LinkedParsableSegment -> ParseManager -> ParseManager
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

    -- \| Assists in getting the handle of a reciever, returning any active handle before returning a fresh one
    getHandle :: Reciever -> ParseManager -> ParserHandle
    getHandle reciever parser =
      case M.lookup (rid reciever) (activeRecievers parser) of
        Nothing -> def reciever -- Default handle for the reciever
        Just handle -> handle -- Existing active handle

    -- \| Updates the reciever map with new state
    updateRecievers :: Maybe ParserHandle -> RecieverID -> RecieverMap -> RecieverMap
    updateRecievers Nothing rid oldMap = M.delete rid oldMap
    updateRecievers (Just handle) rid oldMap = M.insert rid handle oldMap