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

  type LanguageDef = GenLanguageDef<CharString>

  type GenTokenParser<'T, 'U> = {
    Identifier: Parser<'T, CharString>
    Reserved: string -> Parser<'T, unit>
    Operator: Parser<'T, CharString>
    ReservedOp: string -> Parser<'T, unit>
    CharLiteral: Parser<'T, char>
    StringLiteral: Parser<'T, CharString>
    Natural: Parser<'T, int>
    Integer: Parser<'T, int>
    Float: Parser<'T, float>
    NaturalOrFloat: Parser<'T, Choice<int, float>>
    Decimal: Parser<'T, decimal>
    Hexadecimal: Parser<'T, decimal>
    Octal: Parser<'T, decimal>
    Symbol: string -> Parser<'T, CharString>
    Lexeme: Parser<'T, 'U> -> Parser<'T, 'U>
    WhiteSpace: Parser<'T, unit>
    Parens: Parser<'T, 'U> -> Parser<'T, 'U>
    Braces: Parser<'T, 'U> -> Parser<'T, 'U>
    Angles: Parser<'T, 'U> -> Parser<'T, 'U>
    Brackets: Parser<'T, 'U> -> Parser<'T, 'U>
    Squares: Parser<'T, 'U> -> Parser<'T, 'U>
    Semi: Parser<'T, CharString>
    Comma: Parser<'T, CharString>
    Colon: Parser<'T, CharString>
    Dot: Parser<'T, CharString>
    SemiSep: Parser<'T, 'U> -> Parser<'T, 'U list>
    SemiSep1: Parser<'T, 'U> -> Parser<'T, 'U list>
    CommaSep: Parser<'T, 'U> -> Parser<'T, 'U list>
    CommaSep1: Parser<'T, 'U> -> Parser<'T, 'U list>
  }

  type TokenParser<'TResult> = GenTokenParser<CharString, 'TResult>

  open System
  open Attoparsec.String
  open Helper

  module private TokenParserImpl =

    let simpleSpace = skipMany1 (satisfy (char >> Char.IsWhiteSpace))

    let oneLineComment languageDef = parser {
      let! _ = string_ languageDef.CommentLine
      do! skipMany (satisfy ((<>) '\n'))
      return ()
    }

    let oneOf s = oneOf (s |> CharString.toString)
    let noneOf s = noneOf (s |> CharString.toString)

    let startEnd languageDef =
      languageDef.CommentStart
      |> CharString.append languageDef.CommentEnd

    let rec inCommentSingle languageDef =
      parser { let! _ = string_ languageDef.CommentEnd in return () }
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
      parser { let! _ = string_ languageDef.CommentEnd in return () }
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

    and multiLineComment languageDef = (string_ languageDef.CommentStart) >>. (inComment languageDef)

    let noLine languageDef = CharString.isEmpty languageDef.CommentLine
    let noMulti languageDef = CharString.isEmpty languageDef.CommentStart

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

    let symbol languageDef name = lexeme languageDef (string_ (CharString.ofString name))

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
      let parseEsc (c, code) = char_ c |> map (fun _ -> code)
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
      |> map (int >> decimal)
    let octDigit =
      satisfy (tryParse Globalization.NumberStyles.Number) <?> "octal digit"
      |> map (int >> decimal)

    let charNum = parser {
      let! code =
        decimal_
        <|> (char_ 'o' >>= (fun _ -> number 8M octDigit))
        <|> (char_ 'x' >>= (fun _ -> number 16M hexDigit))
      return char code
    }

    let ascii2codes =
      ["BS";"HT";"LF";"VT";"FF";"CR";"SO";"SI";"EM";"FS";"GS";"RS";"US";"SP"]
      |> List.map CharString.ofString
    let ascii3codes =
      ["NUL";"SOH";"STX";"ETX";"EOT";"ENQ";"ACK";"BEL";"DLE";"DC1";"DC2";"DC3";"DC4";"NAK";"SYN";"ETB"; "CAN";"SUB";"ESC";"DEL"]
      |> List.map CharString.ofString

    let ascii2 =
      ['\x08';'\x09';'\x0a';'\x0b';'\x0c';'\x0d';'\x0e';'\x0f';'\x19';'\x1c';'\x1d';'\x1e';'\x1f';'\x20']
    let ascii3 =
      ['\x00';'\x01';'\x02';'\x03';'\x04';'\x05';'\x06';'\x07';'\x10';'\x11';'\x12';'\x13';'\x14';'\x15';'\x16';'\x17';'\x18';'\x1a';'\x1b';'\x7f']

    let asciiMap = List.zip (List.append ascii3codes ascii2codes) (List.append ascii3 ascii2)

    let  charAscii =
      let parseAscii (asc, code) = string_ asc |> map (fun _ -> code)
      choice (List.map parseAscii asciiMap)

    let charControl = parser {
      let! _ = char_ '^'
      let! code = satisfy (char >> inClass "A-Z")
      return char (int code - int 'A')
    }

    let escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

    let charEscape = parser {
      let! _ = char_ '\\'
      return! escapeCode
    }

    let charLetter = satisfy (fun c -> (c <> '\'') && (c <> '\\') && (c > '\026')) |> map char
    
    let characterChar = charLetter <|> charEscape <?> "literal character"
    let charLiteral languageDef =
      lexeme languageDef (between (char_ '\'') (char_ '\'' <?> "end of character") characterChar ) <?> "character"

    let stringLetter = satisfy (fun c -> (c <> '"') && (c <> '\\') && (c > '\026')) |> map char

    let escapeEmpty = char_ '&'
    let escapeGap = parser {
      let! _= many1 (satisfy (char >> Char.IsWhiteSpace))
      return! char_ '\\' <?> "end of string gap"
    }
    let stringEscape = parser {
      let! _ = char_ '\\'
      return!
        (escapeGap |> map (fun _ -> None))
        <|> (escapeEmpty |> map (fun _ -> None))
        <|> (escapeCode |> map Some)
    }

    let stringChar =
      (stringLetter |> map Some) <|> stringEscape <?> "string character"

    let maybe n f = function
      | None -> n
      | Some x -> f x

    let stringLiteral languageDef =
      lexeme languageDef (parser {
        let! str = between (char_ '"') (char_ '"' <?> "end of string") (many stringChar)
        return List.foldBack (maybe id CharString.cons) str CharString.empty
      } <?> "literal string")

    let hexadecimal = oneOf (CharString.ofString "xX") >>. number 16M hexDigit
    let octal = oneOf (CharString.ofString "oO") >>. number 8M octDigit

    let zeroNumber =
      parser {
        let! _ = char_ '0'
        return! hexadecimal <|> octal <|> decimal_ <|> ok 0M
      } <?> ""

    let nat = zeroNumber <|> decimal_

    let sign =
      (char_ '-' >>. ok (~-))
      <|> (char_ '+' >>. ok id)
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
        let! _ = oneOf (CharString.ofString "eE")
        let! f = sign
        let! e = decimal_ <?> "exponent"
        return power (f e)
      } <?> "exponent"

    let digit = satisfy (char >> Char.IsDigit)

    let fraction =
      let op d f = (f + (float d)) / 10.0
      parser {
        let _ = char_ '.'
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

    let fractFloat n = fractExponent n |> map Choice1Of2

    let decimalFloat = parser {
      let! n = decimal_
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

    let natFloat = char_ '0' >>. zeroNumFloat <|> decimalFloat

    let floating = decimal_ >>= fractExponent

    let naturalOrFloat languageDef = lexeme languageDef (natFloat) <?> "number"

    let float_ languageDef = lexeme languageDef floating <?> "float"
    let integer languageDef = lexeme languageDef (int_ languageDef) <?> "integer"
    let natural languageDef = lexeme languageDef nat <?> "natural"

    let notFollowedBy p =
      p >>= (fun c -> error ("notFollowedBy " + c.ToString())) <|> ok ()

    let reservedOp languageDef name =
      lexeme languageDef (parser {
        let! _ = string_ name
        return! notFollowedBy (languageDef.OpLetter) <?> ("end of " + (CharString.toString name))
      })

    let oper languageDef =
      parser {
        let! c = languageDef.OpStart
        let! cs = many languageDef.OpLetter
        return CharString.cons c (CharString.ofList cs)
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
          if isReservedOp languageDef name then error ("reserved operator " + (CharString.toString name))
          else ok name
      })

    let caseString languageDef name =
      let caseChar c =
        if inClass "a-zA-Z" c then char_ (Char.ToLower c) <|> char_ (Char.ToUpper c)
        else char_ c
      let rec walk t =
        if CharString.isEmpty t then ok ()
        else
          let c, cs = CharString.head t, CharString.tail t
          (caseChar (char c) <?> name) >>. walk cs
      let name = CharString.ofString name
      if languageDef.CaseSensitive then string_ name
      else parser {
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
        |> List.map CharString.toString
        |> List.map (fun (s: string) -> s.ToUpper())
        |> List.sort
        |> List.map CharString.ofString

    let isReservedName languageDef (name: string) =
      let caseName =
        if languageDef.CaseSensitive then name
        else name.ToLower()
      isReserved (theReservedNames languageDef) (CharString.ofString caseName)

    let ident languageDef =
      parser {
        let! c = languageDef.IdentStart
        let! cs = many languageDef.IdentLetter
        return CharString.cons c (CharString.ofList cs)
      } <?> "identifier"

    let identifier languageDef =
      lexeme languageDef (parser {
        let! name = ident languageDef
        let name' = CharString.toString name
        return!
          if isReservedName languageDef name' then error ("reserved word " + name')
          else ok name
      })

  open TokenParserImpl

  let makeTokenParser (languageDef: LanguageDef) : TokenParser<_> = {
    Identifier = identifier languageDef
    Reserved = reserved languageDef
    Operator = operator languageDef
    ReservedOp = CharString.ofString >> (reservedOp languageDef)
    CharLiteral = charLiteral languageDef
    StringLiteral = stringLiteral languageDef
    Natural = natural languageDef |> map int
    Integer = integer languageDef |> map int
    Float = float_ languageDef |> map float
    NaturalOrFloat = naturalOrFloat languageDef |> map (function Choice1Of2 a -> Choice1Of2 (int a) | Choice2Of2 b -> Choice2Of2 (float b))
    Decimal = decimal_
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
        CommentStart   = CharString.empty
        CommentEnd     = CharString.empty
        CommentLine    = CharString.empty
        NestedComments = true
        IdentStart     = letter <|> (char_ '_')
        IdentLetter    = alphaNum <|> (String.oneOf "_'")
        OpStart        = op
        OpLetter       = op
        ReservedOpNames= []
        ReservedNames  = []
        CaseSensitive  = true
      }
