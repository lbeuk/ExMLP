module Data.ParserInternals.Reciever where

import Data.ParserInternals.ParserHandle (ParserHandle)

data Reciever = Reciever
  { rid :: RecieverID,
    def :: ParserHandle
  }

data RecieverID
  = Socket
  deriving (Eq, Ord)