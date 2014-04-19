namespace Attoparsec

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
  let inline private head (b: byte []) = b.[0]
  let inline private tail (b: byte []) = b |> Seq.skip 1 |> Seq.toArray
  let private splitAt n (b: byte []) =
    (b |> Seq.take n |> Seq.toArray, b |> Seq.skip n |> Seq.toArray)

  let elem p what = elem length head tail p what

  let satisfy p = satisfy length head tail p

  let skip p what = skip length head tail p what

  let private dropWhile p (b: byte []) = b |> Seq.skipWhile p |> Seq.toArray

  let skipWhile p = skipWhile monoid dropWhile p

  let takeWith n p what = takeWith length splitAt n p what

  let take n = take length splitAt n

  let anyByte: Parser<byte [], byte> = satisfy (fun _ -> true)

  let notByte c = (satisfy ((<>) c)).As("not '" + (string c) + "'")

  let private span pred (b: byte []) =
    let t = b |> Seq.takeWhile pred |> Seq.toList
    let u = b |> Seq.skipWhile pred |> Seq.toArray
    (t, u)

  let takeWhile (p: _ -> bool) : Parser<byte [], byte []> =
    takeWhile monoid span p |> map List.toArray

  let takeRest = takeRest monoid

  let takeText = takeText monoid List.fold

  let byte_ c = elem ((=) c) (Some ("'" + (string c) + "'"))
  let bytes (b: byte []) = takeWith (Array.length b) ((=) b) (Some (b.ToString()))

  let takeWhile1 p : Parser<byte [], byte[]> =
    takeWhile1 monoid span p |> map List.toArray

  let scan s p = scan monoid head tail (fun n (x: byte []) -> x |> Seq.skip n |> Seq.toArray) s p

  let parse (m: Parser<byte [], _>) init = m.Parse(monoid, init)

  let parseAll m init = parse (phrase m) init
