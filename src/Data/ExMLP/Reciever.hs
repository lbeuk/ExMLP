module Data.ExMLP.Reciever where

import Data.ExMLP.ParserHandle (ParserHandle)

data Reciever out = Reciever
  { rid :: RecieverID,
    def :: ParserHandle out
  }

data RecieverID
  = Socket
  deriving (Eq, Ord)