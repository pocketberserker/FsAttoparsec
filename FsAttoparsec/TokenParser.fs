namespace Attoparsec

// port from http://hackage.haskell.org/package/parsec-3.1.5/docs/Text-Parsec-Token.html

module TokenParser =

  type GenLanguageDef<'T> = {
    CommentStart: string
    CommentEnd: string
    CommentLine: string
    NestedComments: bool
    IdentStart: Parser<'T, char>
    IdentLetter: Parser<'T, char>
    OpStart: Parser<'T, char>
    OpLetter: Parser<'T, char>
    ReservedNames: string list
    ReservedOpNames: string list
    caseSensitive: bool
  }

  type LanguageDef = GenLanguageDef<string>

  type GenTokenParser<'T, 'U> = {
    Identifier: Parser<'T, string>
    Reserved: Parser<'T, unit>
    Operator: Parser<'T, string>
    reservedOp: string -> Parser<'T, unit>
    CharLiteral: Parser<'T, char>
    StringLiteral: Parser<'T, string>
    Natural: Parser<'T, int>
    Integer: Parser<'T, int>
    Float: Parser<'T, float>
    NaturalOrFloat: Parser<'T, Choice<int, float>>
    Decimal: Parser<'T, decimal>
    Hexadecimal: Parser<'T, decimal>
    Octal: Parser<'T, decimal>
    Symbol: string -> Parser<'T, string>
    Lexeme: Parser<'T, 'U> -> Parser<'T, 'U>
    WhiteSpace: Parser<'T, unit>
    Parens: Parser<'T, 'U> -> Parser<'T, 'U>
    Braces: Parser<'T, 'U> -> Parser<'T, 'U>
    Angles: Parser<'T, 'U> -> Parser<'T, 'U>
    Brackets: Parser<'T, 'U> -> Parser<'T, 'U>
    Squares: Parser<'T, 'U> -> Parser<'T, 'U>
    Semi: Parser<'T, string>
    Comma: Parser<'T, string>
    Colon: Parser<'T, string>
    Dot: Parser<'T, string>
    SemiSep: Parser<'T, 'U> -> Parser<'T, 'U list>
    SemiSep1: Parser<'T, 'U> -> Parser<'T, 'U list>
    CommaSep: Parser<'T, 'U> -> Parser<'T, 'U list>
    CommaSep1: Parser<'T, 'U> -> Parser<'T, 'U list>
  }
