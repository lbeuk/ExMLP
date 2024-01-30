module Data.ExMLP.ParseResult where
  
import Data.ExMLP.ParsableSegment ( ParsableSegment )
import Data.Sequence (Seq)

-- | Result of a parse 
data ParseResult out = ParseResult out (Seq ParsableSegment)

type ParseResults out = Seq (ParseResult out)