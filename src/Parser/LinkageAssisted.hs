module Parser.LinkageAssisted where

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

type LinkageAssistedParserFn out a = a -> ParsableSegment -> (ParseResults out, Maybe a)

makeHandle :: LinkageAssistedParserFn out a -> a -> ParserHandle out
makeHandle helper def =
  let linkageDef =
        LinkageHelper
          { linked = empty,
            dat = def
          }
   in Unassisted.makeHandle (linkageHelper helper) linkageDef

linkageHelper :: LinkageAssistedParserFn out a -> Unassisted.UnassistedParserFn out (LinkageHelper out a)
linkageHelper pf origOuterSt newLinkedSegment =
  let -- Extracts state from linkage helper
      origInnerSt = dat origOuterSt
      -- Appends new data to back linkages
      oldBackLinkage = linked origOuterSt |> newLinkedSegment
      -- Extracts segment from new linked segment
      newUnlinkedSegment = segment newLinkedSegment
      -- Runs inner function:q
      (newUnlinkedRes, newInnerSt) = pf origInnerSt newUnlinkedSegment
      -- Links results to back linkages
      newLinkedRes = mapWithIndex (\i unlinkedRes -> linkParseResult unlinkedRes oldBackLinkage) newUnlinkedRes
      -- Sets back linkages to empty when a result comes through, effectively using them
      newBackLinkage = case length newLinkedRes of
        0 -> empty
        1 -> oldBackLinkage
      -- Updates the linkage helper
      newOuterSt = outerStHelper newInnerSt newBackLinkage
    in (newLinkedRes, newOuterSt)
  where
    outerStHelper :: Maybe a -> Seq (LinkedParsableSegment out) -> Maybe (LinkageHelper out a)
    outerStHelper Nothing _ = Nothing
    outerStHelper (Just innerSt) backrefs =
      Just
        LinkageHelper
          { linked = backrefs,
            dat = innerSt
          }