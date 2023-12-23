module FileParser.ParseFile (parseFile) where

import FileParser (FileParser)
import FileParser.PcapParser (pcapParser)
import Text.Regex.TDFA

fileParsers :: [(String, FileParser)]
fileParsers =
  [ (".pcap\'", pcapParser)
  ]

data ParseArgs = ParseArgs
  { maxPackets :: Maybe Int, -- Max number of packets to parse from file
    nestMetaData :: Maybe Bool -- If true, move the file metadata into each packet
  }
  deriving (Show)

printParsed :: Maybe 

parseWithParser :: String -> ParseArgs -> Maybe FileParser -> String
parseWithParser path args Nothing = "ERROR There is no parser available for file " ++ path
parseWithParser path args (Just parser) = show $ parser

getParser :: [(String, FileParser)] -> String -> Maybe FileParser
getParser [] path = Nothing
getParser ((regex, parser) : t) path = if (path =~ regex :: Bool) then Just parser else getParser t path



-- | Function to take in a file, arguments, and parse
parseFile :: String -> ParseArgs -> String
parseFile path args = parseWithParser path args (getParser path)