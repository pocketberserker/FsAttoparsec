namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open Attoparsec

[<TestFixture>]
module CharStringTest =

  [<Test>]
  let ``append test`` () =
    check <| fun s t ->
      CharString.toString (CharString.append (CharString.ofString s) (CharString.ofString t)) = s + t 

  [<Test>]
  let ``fold test`` () =
    check <| fun s ->
      let actual = CharString.ofString s |> CharString.fold (fun acc c -> acc + (string c)) ""
      actual |> should equal s

  [<Test>]
  let ``head test`` () =
    check <| fun (s: string) ->
      ((not <| System.String.IsNullOrEmpty s) ==>
        lazy (CharString.head (CharString.ofString s) |> char = s.Chars(0)))

  [<Test>]
  let ``cons test`` () =
    check <| fun (s: string) ->
      let s = CharString.ofString s
      ((not <| CharString.isEmpty s) ==>
        lazy ((CharString.cons (CharString.head s) (CharString.tail s)) = s))

  [<TestCase("", 0, "", "")>]
  [<TestCase("hoge", -10, "", "hoge")>]
  [<TestCase("hoge", -1,  "", "hoge")>]
  [<TestCase("hoge", 0,   "", "hoge")>]
  [<TestCase("hoge", 1,   "h", "oge")>]
  [<TestCase("hoge", 2,   "ho", "ge")>]
  [<TestCase("hoge", 3,   "hog", "e")>]
  [<TestCase("hoge", 4,   "hoge", "")>]
  [<TestCase("hoge", 5,   "hoge", "")>]
  [<TestCase("hoge", 100, "hoge", "")>]
  let ``splitAt test`` (str: string, pos: int, expectedFront: string, expectedBack: string) =
    let str = CharString.ofString str
    let expectedFront = CharString.ofString expectedFront
    let expectedBack = CharString.ofString expectedBack
    let act = str |> CharString.splitAt pos
    act |> should equal (expectedFront, expectedBack)

  [<Test>]
  let ``span test`` () =
    check <| fun (s: string) ->
      let text = CharString.ofString s
      let f = char >> System.Char.IsNumber
      let actual =
        text
        |> CharString.span f
        |> (fun (x, y) -> CharString.toString x, CharString.toString y)
      let f = System.Char.IsNumber
      let expected =
        (System.String(Seq.takeWhile f s |> Seq.toArray),
          System.String(Seq.skipWhile f s |> Seq.toArray))
      actual |> should equal expected
