namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open System
open Attoparsec
open Attoparsec.String

[<TestFixture>]
module ParserTest =

  let cons w s = (string w) + s

  [<Test>]
  let ``satisfy`` () =
    check <| fun w s ->
      let actual =
        cons w s
        |> parse (satisfy (fun x -> x <= w))
        |> ParseResult.option
      actual = Some w

  [<Test>]
  let ``char`` () =
    check <| fun w s ->
      let actual =
        cons w s
        |> parse (char_ w)
        |> ParseResult.option
      actual = Some w

  [<Test>]
  let ``anyChar`` () =
    check <| fun s ->
      let p = (parse anyChar s).Option
      match List.ofSeq s with | [] -> p = None | x::_ -> p = Some x

  [<Test>]
  let ``notChar`` () =
    check <| fun w s ->
      (not <| String.IsNullOrEmpty(s)) ==>
        lazy (let v = s.Chars(0) in (parse (notChar w) s).Option = (if v = w then None else Some v))

  [<Test>]
  let ``string`` () =
    check <| fun s t ->
      (parse (string_ s) (s + t)).Option = Some s

  [<Test>]
  let ``takeCount`` () =
    check <| fun k s ->
      (k >= 0) ==> lazy (match (parse (take k) s).Option with | None -> k > String.length s | Some _ -> k <= String.length s)

  module List =

    let span pred l =
      let l = Seq.ofList l
      let t = l |> Seq.takeWhile pred |> Seq.toList
      let u = l |> Seq.skipWhile pred |> Seq.toList
      (t, u)

  [<Test>]
  let ``takeWhile`` () =
    check <| fun w s ->
      let (h, t) = List.span ((=) w) (List.ofSeq s)
      let h = System.String(Array.ofList h)
      let t = System.String(Array.ofList t)
      s
      |> parseOnly (parser {
        let! hp = takeWhile ((=) w)
        let! tp = takeText
        return (hp, tp)
      })
      |> ((=) (Choice1Of2 (h, t)))

  [<Test>]
  let ``takeWhile1`` () =
    check <| fun w s ->
      let sp = cons w s
      let (h, t) = List.span (fun x -> x <= w) (List.ofSeq sp)
      let h = System.String(Array.ofList h)
      let t = System.String(Array.ofList t)
      sp
      |> parseOnly (parser {
        let! hp = takeWhile1 (fun x -> x <= w)
        let! tp = takeText
        return (hp, tp)
      })
      |> ((=) (Choice1Of2 (h, t)))

  [<Test>]
  let ``takeWhile1 empty`` () =
    ""
    |> parse (Attoparsec.String.takeWhile1 (fun _ -> true))
    |> ParseResult.option
    |> should equal None

  [<Test>]
  let ``endOfInput`` () =
    check <| fun s ->
      s |> parseOnly endOfInput = (if String.IsNullOrEmpty s then Choice1Of2 () else Choice2Of2 "endOfInput")
