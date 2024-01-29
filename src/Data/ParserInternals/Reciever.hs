module Data.ParserInternals.Reciever where

import Data.ParserInternals.ParserHandle (ParserHandle)

data Reciever out = Reciever
  { rid :: RecieverID,
    def :: ParserHandle out
  }

data RecieverID
  = Socket
  deriving (Eq, Ord)