module Parser.FakeParser where
import Parser (ParserFn)

type FakeParserState = State Int

fakeParser :: ParserFn

fakeParserInner :: 