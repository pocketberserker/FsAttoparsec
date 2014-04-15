namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open System
open Attoparsec.Parser

[<TestFixture>]
module ParserTest =

  let cons w (s: string) : string = System.String(w :: (List.ofSeq s) |> List.toArray)

  [<Test>]
  let ``satisfy`` () =
    check <| fun w s -> (satisfy (fun x -> x <= w)).Parse(cons w s).Option = Some w

  [<Test>]
  let ``char`` () =
    check <| fun w s -> (char_ w).Parse(cons w s).Option = Some w

  [<Test>]
  let ``anyChar`` () =
    check <| fun s ->
      let p = anyChar.Parse(s).Option
      match List.ofSeq s with | [] -> p = None | x::_ -> p = Some x

  [<Test>]
  let ``notChar`` () =
    check <| fun w s ->
      (not <| String.IsNullOrEmpty(s)) ==> lazy (let v = s.Chars(0) in (notChar w).Parse(s).Option = (if v = w then None else Some v))

  [<Test>]
  let ``string`` () =
    check <| fun s t -> (string_ s).Parse(s + t).Option = Some s

  [<Test>]
  let ``takeCount`` () =
    check <| fun k s ->
      (k >= 0) ==> lazy (match (take k).Parse(s).Option with | None -> k > String.length s | Some _ -> k <= String.length s)

  [<Test>]
  let ``takeWhile`` () =
    check <| fun w s ->
      let (h, t) = List.partition ((=) w) (List.ofSeq s)
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
      let (h, t) = List.partition (fun x -> x <= w) (List.ofSeq sp)
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
    (Attoparsec.Parser.takeWhile1 (fun _ -> true)).Parse("").Option |> should equal None

  [<Test>]
  let ``endOfInput`` () =
    check <| fun s ->
      s |> parseOnly endOfInput = (if String.IsNullOrEmpty s then Choice1Of2 () else Choice2Of2 "endOfInput")
