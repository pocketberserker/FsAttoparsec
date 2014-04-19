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

  let cons b xs = Array.append [| b |] xs

  [<Test>]
  let ``satisfy`` () =
    check <| fun b xs ->
      let actual =
        cons b xs
        |> parse (satisfy (fun x -> x <= b))
        |> ParseResult.option
      actual = Some b

  [<Test>]
  let ``byte`` () =
    check <| fun b xs ->
      let actual =
        cons b xs
        |> parse (byte_ b)
        |> ParseResult.option
      actual = Some b

  [<Test>]
  let ``anyByte`` () =
    check <| fun xs ->
      let p = (parse anyByte xs).Option
      match List.ofArray xs with | [] -> p = None | x::_ -> p = Some x

  [<Test>]
  let ``notByte`` () =
    check <| fun b xs ->
      ((not <| Array.isEmpty xs) ==>
        lazy (let v = xs.[0] in (parse (notByte b) xs).Option = (if v = b then None else Some v)))

  [<Test>]
  let ``bytes`` () =
    check <| fun s t ->
      (parse (bytes s) (Array.append s t)).Option = Some s

  [<Test>]
  let ``takeCount`` () =
    check <| fun k s ->
      (k >= 0) ==> lazy (match (parse (take k) s).Option with | None -> k > Array.length s | Some _ -> k <= Array.length s)

  module Array =

    let span pred (b: byte []) =
      let t = b |> Seq.takeWhile pred |> Seq.toList
      let u = b |> Seq.skipWhile pred |> Seq.toArray
      (t, u)

  [<Test>]
  let ``takeWhile`` () =
    check <| fun b xs ->
      let (h, t) = Array.span ((=) b) xs
      let h = Array.ofList h
      xs
      |> parseOnly (parser {
        let! hp = takeWhile ((=) b)
        let! tp = takeText
        return (hp, tp)
      })
      |> ((=) (Choice1Of2 (h, t)))

  [<Test>]
  let ``takeWhile1`` () =
    check <| fun b xs ->
      let sp = cons b xs
      let (h, t) = Array.span (fun x -> x <= b) sp
      let h = Array.ofList h
      sp
      |> parseOnly (parser {
        let! hp = takeWhile1 (fun x -> x <= b)
        let! tp = takeText
        return (hp, tp)
      })
      |> ((=) (Choice1Of2 (h, t)))

  [<Test>]
  let ``takeWhile1 empty`` () =
    [||]
    |> parse (Binary.takeWhile1 (fun _ -> true))
    |> ParseResult.option
    |> should equal None

  [<Test>]
  let ``endOfInput`` () =
    check <| fun s ->
      s |> parseOnly endOfInput = (if Array.isEmpty s then Choice1Of2 () else Choice2Of2 "endOfInput")
