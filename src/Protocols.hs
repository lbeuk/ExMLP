module Protocols where

data NetProtocol
  = NoProtocol
  | NetProtocolL2 L2Protocol
  | NetProtocolL3 L2Protocol
  | NetProtocolL4 L4Protocol

-- Data Link Protocols
data L2Protocol
  = Ethernet

-- Network Layer Protocols
data L3Protocol
  = IP -- Redirects to IPv4 or IPv6
  | IPv4
  | IPv6

-- Transport Layer Protocols
data L4Protocol
  = UDP

-- Application Layer Protocols
data L5Protocol
  = HTTP