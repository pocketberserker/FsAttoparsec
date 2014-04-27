namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open System
open Attoparsec
open Attoparsec.String
open Attoparsec.Token
open Attoparsec.Expr

[<TestFixture>]
module TokenParserTest =

  let exprDef = {
    Language.empty with OpLetter = oneOf "+-*/"
  }

  let lexer<'a> : TokenParser<'a> = makeTokenParser exprDef

  let rec term = lazy (lexer.Parens expr <|> lexer.Natural)
  and expr =
    let op sym fn = Infix(lexer.ReservedOp sym >>. ok fn, AssocLeft)
    let table = [
      [op "*" (*); op "/" (/)];
      [op "+" (+); op "-" (-)]
    ]
    buildExpressionParser table term

  let exprParser = lexer.WhiteSpace >>. expr

  let parseExpr input = parse exprParser input |> ParseResult.feed ""

  [<TestCase("1 + 2", 3)>]
  [<TestCase("1 - 1", 0)>]
  [<TestCase("2 * 3", 6)>]
  [<TestCase("4 / 2", 2)>]
  [<TestCase("1 + 2 * 3", 7)>]
  let ``expression test`` input expected =
    match parseExpr input with
    | Done(_, actual) -> actual |> should equal expected
    | _ -> Assert.Fail()
