namespace Attoparsec

// port from http://hackage.haskell.org/package/parsec-3.1.5/docs/ByteString-Parsec-Token.html

module Token =

  type GenLanguageDef<'T> = {
    CommentStart: 'T
    CommentEnd: 'T
    CommentLine: 'T
    NestedComments: bool
    IdentStart: Parser<'T, char>
    IdentLetter: Parser<'T, char>
    OpStart: Parser<'T, char>
    OpLetter: Parser<'T, char>
    ReservedNames: 'T list
    ReservedOpNames: 'T list
    CaseSensitive: bool
  }

  type LanguageDef = GenLanguageDef<BmpString>

  type GenTokenParser<'T, 'U> = {
    Identifier: Parser<'T, BmpString>
    Reserved: string -> Parser<'T, unit>
    Operator: Parser<'T, BmpString>
    ReservedOp: string -> Parser<'T, unit>
    CharLiteral: Parser<'T, char>
    StringLiteral: Parser<'T, BmpString>
    Natural: Parser<'T, int>
    Integer: Parser<'T, int>
    Float: Parser<'T, float>
    NaturalOrFloat: Parser<'T, Choice<int, float>>
    Decimal: Parser<'T, decimal>
    Hexadecimal: Parser<'T, decimal>
    Octal: Parser<'T, decimal>
    Symbol: string -> Parser<'T, BmpString>
    Lexeme: Parser<'T, 'U> -> Parser<'T, 'U>
    WhiteSpace: Parser<'T, unit>
    Parens: Parser<'T, 'U> -> Parser<'T, 'U>
    Braces: Parser<'T, 'U> -> Parser<'T, 'U>
    Angles: Parser<'T, 'U> -> Parser<'T, 'U>
    Brackets: Parser<'T, 'U> -> Parser<'T, 'U>
    Squares: Parser<'T, 'U> -> Parser<'T, 'U>
    Semi: Parser<'T, BmpString>
    Comma: Parser<'T, BmpString>
    Colon: Parser<'T, BmpString>
    Dot: Parser<'T, BmpString>
    SemiSep: Parser<'T, 'U> -> Parser<'T, 'U list>
    SemiSep1: Parser<'T, 'U> -> Parser<'T, 'U list>
    CommaSep: Parser<'T, 'U> -> Parser<'T, 'U list>
    CommaSep1: Parser<'T, 'U> -> Parser<'T, 'U list>
  }

  type TokenParser<'TResult> = GenTokenParser<BmpString, 'TResult>

  open System
  open Attoparsec.String
  open Helper

  module private TokenParserImpl =

    let simpleSpace = skipMany1 (satisfy Char.IsWhiteSpace)

    let oneLineComment languageDef =
      let commentLine = BmpString.toString languageDef.CommentLine
      parser {
      let! _ = pstring commentLine
      do! skipMany (satisfy ((<>) '\n'))
      return ()
    }

    let oneOf s = oneOf (s |> BmpString.toString)
    let noneOf s = noneOf (s |> BmpString.toString)

    let startEnd languageDef =
      languageDef.CommentStart
      |> BmpString.append languageDef.CommentEnd

    let rec inCommentSingle languageDef =
      let commentEnd = BmpString.toString languageDef.CommentEnd
      (pstring commentEnd >>. ok ())
      <|> parser {
        do! skipMany1 (noneOf (startEnd languageDef))
        return! inCommentSingle languageDef }
      <|> parser {
        let! _ = oneOf (startEnd languageDef)
        return! inCommentSingle languageDef }
      <?> "end of comment"

    let rec inComment languageDef =
      if languageDef.NestedComments then inCommentMulti languageDef
      else inCommentSingle languageDef

    and inCommentMulti languageDef =
      let commentEnd = BmpString.toString languageDef.CommentEnd
      (pstring commentEnd >>. ok ())
      <|> parser {
        do! multiLineComment languageDef
        return! inCommentMulti languageDef }
      <|> parser {
        do! skipMany1 (noneOf (startEnd languageDef))
        return! inCommentMulti languageDef }
      <|> parser {
        let! _ = oneOf (startEnd languageDef)
        return! inCommentMulti languageDef }
      <?> "end of comment"

    and multiLineComment languageDef =
      let commentStart = BmpString.toString languageDef.CommentStart
      (pstring commentStart) >>. (inComment languageDef)

    let noLine languageDef = BmpString.isEmpty languageDef.CommentLine
    let noMulti languageDef = BmpString.isEmpty languageDef.CommentStart

    let whiteSpace languageDef =
      match noLine languageDef, noMulti languageDef with
      | true, true -> skipMany (simpleSpace <?> "")
      | true, false -> skipMany (simpleSpace <|> multiLineComment languageDef <?> "")
      | false, true -> skipMany (simpleSpace <|> oneLineComment languageDef <?> "")
      | false, false -> skipMany (simpleSpace <|> oneLineComment languageDef <|> multiLineComment languageDef <?> "")

    let lexeme languageDef p = parser {
      let! x = p
      do! whiteSpace languageDef
      return x
    }

    let symbol languageDef name = lexeme languageDef (pstring name)

    let parens languageDef p = between (symbol languageDef "(") (symbol languageDef ")") p
    let braces languageDef p = between (symbol languageDef "{") (symbol languageDef "}") p
    let angles languageDef p = between (symbol languageDef "<") (symbol languageDef ">") p
    let brackets languageDef p = between (symbol languageDef "[") (symbol languageDef "]") p

    let semi languageDef = symbol languageDef ";"
    let comma languageDef = symbol languageDef ","
    let dot languageDef = symbol languageDef "."
    let colon languageDef = symbol languageDef ":"

    let commaSep languageDef p = sepBy p (comma languageDef)
    let semiSep languageDef p = sepBy p (semi languageDef)

    let commaSep1 languageDef p = sepBy1 p (comma languageDef)
    let semiSep1 languageDef p = sepBy1 p (semi languageDef)

    let escMap = Seq.zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'" |> Seq.toList

    let charEsc =
      let parseEsc (c, code) = pchar c >>% code
      choice (List.map parseEsc escMap)

    let number base_ baseDigit = parser {
      let! digits = many1 baseDigit
      let n = List.fold (fun x d -> base_ * x + d) 0M digits
      return n
    }

    let tryParse (style: Globalization.NumberStyles) c =
      match Decimal.TryParse(string c, Globalization.NumberStyles.HexNumber, Globalization.CultureInfo.CurrentCulture) with
      | result, _ -> result

    let hexDigit =
      satisfy (tryParse Globalization.NumberStyles.HexNumber) <?> "hexadecimal digit"
      |>> (int >> decimal)
    let octDigit =
      satisfy (tryParse Globalization.NumberStyles.Number) <?> "octal digit"
      |>> (int >> decimal)

    let charNum = parser {
      let! code =
        pdecimal
        <|> (pchar 'o' >>. number 8M octDigit)
        <|> (pchar 'x' >>. number 16M hexDigit)
      return char code
    }

    let ascii2codes =
      ["BS";"HT";"LF";"VT";"FF";"CR";"SO";"SI";"EM";"FS";"GS";"RS";"US";"SP"]
    let ascii3codes =
      ["NUL";"SOH";"STX";"ETX";"EOT";"ENQ";"ACK";"BEL";"DLE";"DC1";"DC2";"DC3";"DC4";"NAK";"SYN";"ETB"; "CAN";"SUB";"ESC";"DEL"]

    let ascii2 =
      ['\x08';'\x09';'\x0a';'\x0b';'\x0c';'\x0d';'\x0e';'\x0f';'\x19';'\x1c';'\x1d';'\x1e';'\x1f';'\x20']
    let ascii3 =
      ['\x00';'\x01';'\x02';'\x03';'\x04';'\x05';'\x06';'\x07';'\x10';'\x11';'\x12';'\x13';'\x14';'\x15';'\x16';'\x17';'\x18';'\x1a';'\x1b';'\x7f']

    let asciiMap = List.zip (List.append ascii3codes ascii2codes) (List.append ascii3 ascii2)

    let  charAscii =
      let parseAscii (asc, code) = pstring asc >>% code
      choice (List.map parseAscii asciiMap)

    let charControl = parser {
      let! _ = pchar '^'
      let! code = satisfy (inClass "A-Z")
      return char (int code - int 'A')
    }

    let escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

    let charEscape = parser {
      let! _ = pchar '\\'
      return! escapeCode
    }

    let charLetter = satisfy (fun c -> (c <> '\'') && (c <> '\\') && (c > '\026'))

    let characterChar = charLetter <|> charEscape <?> "literal character"
    let charLiteral languageDef =
      lexeme languageDef (between (pchar '\'') (pchar '\'' <?> "end of character") characterChar ) <?> "character"

    let stringLetter = satisfy (fun c -> (c <> '"') && (c <> '\\') && (c > '\026'))

    let escapeEmpty = pchar '&'
    let escapeGap = parser {
      let! _= many1 (satisfy Char.IsWhiteSpace)
      return! pchar '\\' <?> "end of string gap"
    }
    let stringEscape = parser {
      let! _ = pchar '\\'
      return!
        (escapeGap >>% None)
        <|> (escapeEmpty >>% None)
        <|> (escapeCode |>> Some)
    }

    let stringChar =
      (stringLetter |>> Some) <|> stringEscape <?> "string character"

    let maybe n f = function
      | None -> n
      | Some x -> f x

    let stringLiteral languageDef =
      lexeme languageDef (parser {
        let! str = between (pchar '"') (pchar '"' <?> "end of string") (many stringChar)
        return List.foldBack (maybe id BmpString.cons) str BmpString.empty
      } <?> "literal string")

    let hexadecimal = oneOf (BmpString.ofString "xX") >>. number 16M hexDigit
    let octal = oneOf (BmpString.ofString "oO") >>. number 8M octDigit

    let zeroNumber =
      parser {
        let! _ = pchar '0'
        return! hexadecimal <|> octal <|> pdecimal <|> ok 0M
      } <?> ""

    let nat = zeroNumber <|> pdecimal

    let sign =
      (pchar '-' >>. ok (~-))
      <|> (pchar '+' >>. ok id)
      <|> ok id

    let int_ languageDef = parser {
      let! f = lexeme languageDef sign
      let! n = nat
      return f n
    }

    let rec pow (a: decimal) (b: decimal) =
      let rec inner acc b =
        if b <= 0M then acc
        else pow (a * acc) (b - 1M)
      pow 1M b

    let exponent' =
      let rec power e =
        if e < 0M then 1.0M / power (-e)
        else pow 10M e
      parser {
        let! _ = oneOf (BmpString.ofString "eE")
        let! f = sign
        let! e = pdecimal <?> "exponent"
        return power (f e)
      } <?> "exponent"

    let digit = satisfy Char.IsDigit

    let fraction =
      let op d f = (f + (float d)) / 10.0
      parser {
        let _ = pchar '.'
        let! digits = many1 digit <?> "fraction"
        return (List.foldBack op digits 0.0)
      } <?> "fraction"

    let fractExponent n =
      parser{
        let! fract = fraction
        let! expo  = option 1.0M exponent'
        return (n + (decimal fract)) * expo
      }
      <|>
      parser {
        let! expo = exponent'
        return n * expo
      }

    let fractFloat n = fractExponent n |>> Choice1Of2

    let decimalFloat = parser {
      let! n = pdecimal
      return! option (Choice2Of2 n) (fractFloat n)
    }

    let zeroNumFloat =
      parser {
        let! n = hexadecimal <|> octal
        return (Choice2Of2 n)
      }
      <|> decimalFloat
      <|> fractFloat 0M
      <|> ok (Choice2Of2 0M)

    let natFloat = pchar '0' >>. zeroNumFloat <|> decimalFloat

    let floating = pdecimal >>= fractExponent

    let naturalOrFloat languageDef = lexeme languageDef (natFloat) <?> "number"

    let float_ languageDef = lexeme languageDef floating <?> "float"
    let integer languageDef = lexeme languageDef (int_ languageDef) <?> "integer"
    let natural languageDef = lexeme languageDef nat <?> "natural"

    let notFollowedBy p =
      p >>= (fun c -> error ("notFollowedBy " + c.ToString())) <|> ok ()

    let reservedOp languageDef name =
      lexeme languageDef (parser {
        let! _ = pstring name
        return! notFollowedBy (languageDef.OpLetter) <?> ("end of " + name)
      })

    let oper languageDef =
      parser {
        let! c = languageDef.OpStart
        let! cs = many languageDef.OpLetter
        return BmpString.cons c (BmpString.ofList cs)
      } <?> "operator"

    let rec isReserved names name =
      match names with
      | [] -> false
      | r::rs ->
        let c = compare r name
        if c < 0 then isReserved rs name
        elif c = 0 then true
        else false

    let isReservedOp languageDef name =
      isReserved (List.sort languageDef.ReservedOpNames) name

    let operator languageDef =
      lexeme languageDef (parser {
        let! name = oper languageDef
        return!
          if isReservedOp languageDef name then error ("reserved operator " + (BmpString.toString name))
          else ok name
      })

    let caseString languageDef name =
      let caseChar c =
        if inClass "a-zA-Z" c then pchar (Char.ToLower c) <|> pchar (Char.ToUpper c)
        else pchar c
      let rec walk t =
        if BmpString.isEmpty t then ok ()
        else
          let c, cs = BmpString.head t, BmpString.tail t
          (caseChar (char c) <?> name) >>. walk cs
      if languageDef.CaseSensitive then pstring name
      else
        let name = BmpString.ofString name
        parser {
          do! walk name
          return name
        }

    let reserved languageDef name =
      lexeme languageDef (parser {
        let! _ = caseString languageDef name
        return! notFollowedBy languageDef.IdentLetter <?> ("end of " + name)
      })

    let theReservedNames languageDef =
      if languageDef.CaseSensitive then List.sort languageDef.ReservedNames
      else
        languageDef.ReservedNames
        |> List.map BmpString.toString
        |> List.map (fun (s: string) -> s.ToUpper())
        |> List.sort
        |> List.map BmpString.ofString

    let isReservedName languageDef (name: string) =
      let (Bmp caseName) =
        if languageDef.CaseSensitive then name
        else name.ToLower()
      isReserved (theReservedNames languageDef) caseName

    let ident languageDef =
      parser {
        let! c = languageDef.IdentStart
        let! cs = many languageDef.IdentLetter
        return BmpString.cons c (BmpString.ofList cs)
      } <?> "identifier"

    let identifier languageDef =
      lexeme languageDef (parser {
        let! name = ident languageDef
        let name' = BmpString.toString name
        return!
          if isReservedName languageDef name' then error ("reserved word " + name')
          else ok name
      })

  open TokenParserImpl

  let makeTokenParser (languageDef: LanguageDef) : TokenParser<_> = {
    Identifier = identifier languageDef
    Reserved = reserved languageDef
    Operator = operator languageDef
    ReservedOp = reservedOp languageDef
    CharLiteral = charLiteral languageDef
    StringLiteral = stringLiteral languageDef
    Natural = natural languageDef |>> int
    Integer = integer languageDef |>> int
    Float = float_ languageDef |>> float
    NaturalOrFloat = naturalOrFloat languageDef |>> (function Choice1Of2 a -> Choice1Of2 (int a) | Choice2Of2 b -> Choice2Of2 (float b))
    Decimal = pdecimal
    Hexadecimal = hexadecimal
    Octal = octal
    Symbol = symbol languageDef
    Lexeme = lexeme languageDef
    WhiteSpace = whiteSpace languageDef
    Parens = parens languageDef
    Braces = braces languageDef
    Angles = angles languageDef
    Brackets = brackets languageDef
    Squares = brackets languageDef
    Semi = semi languageDef
    Comma = comma languageDef
    Colon = colon languageDef
    Dot = dot languageDef
    SemiSep = semiSep languageDef
    SemiSep1 = semiSep1 languageDef
    CommaSep = commaSep languageDef
    CommaSep1 = commaSep1 languageDef
  }

  module Language =

    let empty =
      let op = String.oneOf ":!#$%&*+./<=>?@\\^|-~"
      {
        CommentStart   = BmpString.empty
        CommentEnd     = BmpString.empty
        CommentLine    = BmpString.empty
        NestedComments = true
        IdentStart     = letter <|> (pchar '_')
        IdentLetter    = alphaNum <|> (String.oneOf "_'")
        OpStart        = op
        OpLetter       = op
        ReservedOpNames= []
        ReservedNames  = []
        CaseSensitive  = true
      }
