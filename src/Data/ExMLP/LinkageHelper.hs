module Data.ExMLP.LinkageHelper where
import Data.Sequence (Seq)
import Data.ExMLP.LinkedData (LinkedParsableSegment)

-- | Helper data structure for linkage assisted parsers
data LinkageHelper out a = LinkageHelper {
  linked :: Seq (LinkedParsableSegment out),
  dat :: a
}