namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open Attoparsec

[<TestFixture>]
module BmpStringTest =

  [<Test>]
  let ``append test`` () =
    check <| fun s t ->
      BmpString.toString (BmpString.append (BmpString.ofString s) (BmpString.ofString t)) = s + t 

  [<Test>]
  let ``fold test`` () =
    check <| fun s ->
      let actual = BmpString.ofString s |> BmpString.fold (fun acc c -> acc + (string c)) ""
      actual |> should equal s

  [<Test>]
  let ``head test`` () =
    check <| fun (s: string) ->
      ((not <| System.String.IsNullOrEmpty s) ==>
        lazy (BmpString.head (BmpString.ofString s) |> char = s.Chars(0)))

  [<Test>]
  let ``cons test`` () =
    check <| fun (s: string) ->
      let s = BmpString.ofString s
      ((not <| BmpString.isEmpty s) ==>
        lazy ((BmpString.cons (BmpString.head s) (BmpString.tail s)) = s))

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
    let str = BmpString.ofString str
    let expectedFront = BmpString.ofString expectedFront
    let expectedBack = BmpString.ofString expectedBack
    let act = str |> BmpString.splitAt pos
    act |> should equal (expectedFront, expectedBack)

  [<Test>]
  let ``span test`` () =
    check <| fun (s: string) ->
      let text = BmpString.ofString s
      let f = char >> System.Char.IsNumber
      let actual =
        text
        |> BmpString.span f
        |> (fun (x, y) -> BmpString.toString x, BmpString.toString y)
      let f = System.Char.IsNumber
      let expected =
        (System.String(Seq.takeWhile f s |> Seq.toArray),
          System.String(Seq.skipWhile f s |> Seq.toArray))
      actual |> should equal expected

  open Helper
  
  let monoid = BmpString.monoid
  let mempty = monoid.Mempty
  let mappend x y = monoid.Mappend(x, y)

  [<Test>]
  let ``monoid first law``() =
    check <| fun (Bmp x) ->
      mappend mempty x = x

  [<Test>]
  let ``monoid second law``() =
    check <| fun (Bmp x) ->
      mappend x mempty = x

  [<Test>]
  let ``monoid third law``() =
    check <| fun (Bmp x) (Bmp y) (Bmp z) ->
      mappend x (mappend y z) = mappend (mappend x y) z
