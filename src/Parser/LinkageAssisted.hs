module Parser.LinkageAssisted (LinkageAssistedParserFn, makeHandle) where

import Data.ParserInternals.LinkageHelper (LinkageHelper (..))
import Data.ParserInternals.LinkedData
  ( LinkedParsableSegment (LinkedParsableSegment, segment),
    LinkedParseResult (LinkedParseResult),
    linkParseResult,
  )
import Data.ParserInternals.ParsableSegment (ParsableSegment)
import Data.ParserInternals.ParseResult (ParseResults)
import Data.ParserInternals.ParserHandle (ParserHandle)
import Data.Sequence (Seq, empty, mapWithIndex, (|>))
import qualified Parser.Unassisted as Unassisted

type LinkageAssistedParserFn a = a -> ParsableSegment -> (ParseResults, Maybe a)

makeHandle :: LinkageAssistedParserFn a -> a -> ParserHandle
makeHandle helper def =
  Unassisted.makeHandle (linkageAssistedToUnassisted helper) (LinkageHelper empty def)

linkageAssistedToUnassisted :: LinkageAssistedParserFn a -> Unassisted.UnassistedParserFn (LinkageHelper a)
linkageAssistedToUnassisted pf origOuterSt newLinkedSegment =
  let -- Runs inner function
      (newUnlinkedRes, newInnerSt) = pf (dat origOuterSt) (segment newLinkedSegment)
      -- Appends new data to back linkages
      oldBackLinkage = linked origOuterSt |> newLinkedSegment
      -- Links results to back linkages
      newLinkedRes = mapWithIndex (\i unlinkedRes -> linkParseResult unlinkedRes oldBackLinkage) newUnlinkedRes
      -- Sets back linkages to empty when a result comes through, effectively using them
      newBackLinkage = case length newLinkedRes of
        0 -> empty
        1 -> oldBackLinkage
      -- Updates the linkage helper
      newOuterSt = case newInnerSt of
        Nothing -> Nothing
        Just innerSt -> Just (LinkageHelper newBackLinkage innerSt)
    in (newLinkedRes, newOuterSt)