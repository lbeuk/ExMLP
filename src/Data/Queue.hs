module Data.Queue where

import Control.Monad.State.Lazy

data Queue a = Queue {
  incoming :: [a],
  outgoing :: [a]
}

enqueue :: Queue a -> a -> Queue a
enqueue old val = Queue (val : incoming old) (outgoing old)

dequeue :: Queue a -> (Queue a, Maybe a)
dequeue q@(Queue [] []) = (q, Nothing)
dequeue (Queue inc []) = dequeue (Queue [] (reverse inc))
dequeue (Queue inc (h:t)) = (Queue inc t, Just h)