module Main where

import Data.IPAddr
import Data.CaptureDict
import Data.Map (fromList)


main :: IO ()
main = do
  let x = IPv6_Addr 0x0000 0x1111 0x0000 0x3333 0x4444 0x5555 0x6666 0x7777 :: IPAddr
      y = IPv4_Addr 192 168 1 1
      c = List [Str "Hello", Str "Trashy", List [Str "Girl", Str "1"]]
      a = CaptureDict (fromList [("1", IPAddr x), ("2", IPAddr y), ("trashy", Str "Hello world"), ("test", c)])
      b = List [Str "Hello", Str "Trashy", List [Str "Girl", SubDict a], SubDict a]
      z = CaptureDict (fromList [("1", IPAddr x), ("2", IPAddr y), ("submap", SubDict a), ("list", b)])

  print z
