namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open TestHelper
open Attoparsec

[<TestFixture>]
module BmpStringTest =

  [<Property>]
  let ``append test`` (NonNullString s) (NonNullString t) =
    BmpString.toString (BmpString.append (BmpString.ofString s) (BmpString.ofString t)) = s + t 

  [<Property>]
  let ``fold test`` (NonNullString s) =
    let actual = BmpString.ofString s |> BmpString.fold (fun acc c -> acc + (string c)) ""
    actual = s

  [<Property>]
  let ``head test`` (NonNullString s) =
    ((not <| System.String.IsNullOrEmpty s) ==>
      lazy (BmpString.head (BmpString.ofString s) |> char = s.Chars(0)))

  [<Property>]
  let ``cons test`` (NonNullString s) =
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

  [<Property>]
  let ``span test`` (NonNullString s) =
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
    actual = expected

  open Helper
  
  let monoid = BmpString.monoid
  let mempty = monoid.Mempty
  let mappend x y = monoid.Mappend(x, y)

  [<Property>]
  let ``monoid first law`` (NonNullString (Bmp x)) =
    mappend mempty x = x

  [<Property>]
  let ``monoid second law`` (NonNullString (Bmp x)) =
    mappend x mempty = x

  [<Property>]
  let ``monoid third law`` (NonNullString (Bmp x)) (NonNullString (Bmp y)) (NonNullString (Bmp z)) =
    mappend x (mappend y z) = mappend (mappend x y) z
