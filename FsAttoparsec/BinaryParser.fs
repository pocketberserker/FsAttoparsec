namespace Attoparsec

open System.Diagnostics.Contracts

module Binary =

  let private monoid = { new Monoid<byte []>() with
    member this.Mempty = [||]
    member this.Mappend(s, t) = Array.append s t
  }

  module ParseResult =
    let feed s (result: ParseResult<_, _>) = ParseResult.feed monoid s result
    let done_ (result: ParseResult<_, _>) = ParseResult.done_ monoid result

  let parseOnly parser input = parseOnly monoid parser input

  let ensure n = ensure Array.length n

  let inline private length s = Array.length s
  let inline private head (b: byte []) =
    Contract.Requires(b.Length >= 1)
    b.[0]
  let inline private tail (b: byte []) =
    Contract.Requires(b.Length >= 1)
    if Array.length b = 1 then [||]
    else b.[ 1 .. ]
  let private splitAt n (b: byte []) =
    Contract.Requires(b.Length >= 0)
    if Array.isEmpty b then ([||], [||])
    elif n <= 0 then ([||], b)
    elif n >= b.Length then (b, [||])
    else (b.[ .. n - 1 ], b.[ n .. ])

  let elem p what = elem length head tail p what

  let satisfy p = satisfy length head tail p

  let skip p what = skip length head tail p what

  let private dropWhile p (b: byte []) =
    match Array.tryFindIndex p b with
    | Some i -> splitAt i b |> snd
    | None -> [||]

  let skipWhile p = skipWhile monoid dropWhile p

  let takeWith n p what = takeWith length splitAt n p what

  let take n = take length splitAt n

  let anyByte: Parser<byte [], byte> = satisfy (fun _ -> true)

  let notByte c = (satisfy ((<>) c)).As("not '" + (string c) + "'")

  let private split pred (b: byte []) =
    match Array.tryFindIndex pred b with
    | Some i -> splitAt i b
    | None -> (b, [||])

  let private span pred (b: byte []) = split (not << pred) b

  let takeWhile (p: _ -> bool) : Parser<byte [], byte []> =
    takeWhile monoid span p

  let takeRest = takeRest monoid

  let takeText = takeText monoid List.fold

  let byte_ c = elem ((=) c) (Some ("'" + (string c) + "'"))
  let bytes (b: byte []) = takeWith (Array.length b) ((=) b) (Some (b.ToString()))

  let takeWhile1 p : Parser<byte [], byte[]> =
    takeWhile1 monoid span p

  let scan s p = scan monoid head tail (fun n (x: byte []) -> x |> splitAt n |> fst) s p

  let parse (m: Parser<byte [], _>) init = m.Parse(monoid, init)

  let parseAll m init = parse (phrase m) init
