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

type LinkageAssistedParserFn a = a -> ParsableSegment -> (ParseResults, Maybe a)

makeHandle :: LinkageAssistedParserFn a -> a -> ParserHandle
makeHandle helper def =
  let linkageDef =
        LinkageHelper
          { linked = empty,
            dat = def
          }
   in Unassisted.makeHandle (linkageHelper helper) linkageDef
  where
    linkageHelper :: LinkageAssistedParserFn a -> Unassisted.UnassistedParserFn (LinkageHelper a)
    linkageHelper pf origOuterSt newLinkedSegment =
      let -- Extracts state from linkage helper
          origInnerSt = dat origOuterSt
          -- Appends new data to back linkages
          oldBackLinkage = linked origOuterSt |> newLinkedSegment
          -- Extracts segment from new linked segment
          newUnlinkedSegment = segment newLinkedSegment
          -- Runs inner function
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
        outerStHelper :: Maybe a -> Seq LinkedParsableSegment -> Maybe (LinkageHelper a)
        outerStHelper Nothing _ = Nothing
        outerStHelper (Just innerSt) backrefs =
          Just
            LinkageHelper
              { linked = backrefs,
                dat = innerSt
              }
