namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open System
open Attoparsec
open Attoparsec.Binary

[<TestFixture>]
module BinaryParserTest =

  module Array =
    let cons b xs = Array.append [| b |] xs

  [<Property>]
  let ``satisfy`` b xs =
    let actual =
      Array.cons b xs
      |> parse (satisfy (fun x -> x <= b))
      |> ParseResult.option
    actual = Some b

  [<Property>]
  let ``byte`` b xs =
    let actual =
      Array.cons b xs
      |> parse (pbyte b)
      |> ParseResult.option
    actual = Some b

  [<Property>]
  let ``anyByte`` xs =
    let p = (parse anyByte xs).Option
    match List.ofArray xs with | [] -> p = None | x::_ -> p = Some x

  [<Property>]
  let ``notByte`` b xs =
    ((not <| Array.isEmpty xs) ==>
      lazy (let v = xs.[0] in (parse (notByte b) xs).Option = (if v = b then None else Some v)))

  [<Test>]
  [<Property>]
  let ``bytes`` s t =
    (parse (bytes s) (Array.append s t)).Option = Some (BinaryArray.ofArray s)

  [<Property>]
  let ``takeCount`` k s =
    (k >= 0) ==> lazy (
      match (parse (take k) s).Option with
      | None -> k > Array.length s
      | Some _ -> k <= Array.length s)

  [<Property>]
  let ``takeWhile `` b xs =
    let s = BinaryArray.ofArray xs
    let (h, t) = BinaryArray.span ((=) b) s
    xs
    |> parseOnly (parser {
      let! hp = takeWhile ((=) b)
      let! tp = takeText
      return (hp, tp)
    })
    |> (=) (Choice1Of2 (h, t))

  [<Property>]
  let ``takeWhile1 `` b xs =
    let sp = Array.cons b xs
    let s = BinaryArray.ofArray sp
    let (h, t) = BinaryArray.span (fun x -> x <= b) s
    sp
    |> parseOnly (parser {
      let! hp = takeWhile1 (fun x -> x <= b)
      let! tp = takeText
      return (hp, tp)
    })
    |> (=) (Choice1Of2 (h, t))

  [<Test>]
  let ``takeWhile1 empty`` () =
    [||]
    |> parse (Binary.takeWhile1 (fun _ -> true))
    |> ParseResult.option
    |> should equal None

  [<Property>]
  let ``endOfInput`` s =
    s |> parseOnly endOfInput = (if Array.isEmpty s then Choice1Of2 () else Choice2Of2 "endOfInput")

  [<Property>]
  let ``getPosition `` b xs =
    let sp = Array.cons b xs
    let s = BinaryArray.ofArray sp
    let expected = BinaryArray.takeWhile (fun x -> x <= b) s |> BinaryArray.length
    sp
    |> parseOnly (parser {
      let! hp = takeWhile (fun x -> x <= b)
      return! getPosition
    })
    |> (=) (Choice1Of2 expected)
