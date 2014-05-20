namespace Attoparsec

open System.Diagnostics.Contracts
open Helper

module Binary =

  module ParseResult =
    let feed s (result: ParseResult<_, _>) = ParseResult.feed BinaryArray.monoid s result
    let done_ (result: ParseResult<_, _>) = ParseResult.done_ BinaryArray.monoid result

  let parseOnly parser (Bin input) =
    parseOnly BinaryArray.monoid parser input

  let ensure n = ensure BinaryArray.length n

  let elem p what = elem BinaryArray.length BinaryArray.head BinaryArray.tail p what

  let satisfy p = satisfy BinaryArray.length BinaryArray.head BinaryArray.tail p

  let skip p what = skip BinaryArray.length BinaryArray.head BinaryArray.tail p what

  let skipWhile p = skipWhile BinaryArray.monoid BinaryArray.skipWhile p

  let takeWith n p what = takeWith BinaryArray.length BinaryArray.splitAt n p what

  let take n = take BinaryArray.length BinaryArray.splitAt n

  let anyByte = satisfy (fun _ -> true)

  let notByte c = (satisfy ((<>) c)) |> as_ ("not '" + (string c) + "'")

  let takeWhile (p: _ -> bool) =
    takeWhile BinaryArray.monoid BinaryArray.span p

  let takeRest = takeRest BinaryArray.monoid

  let takeText = takeText BinaryArray.monoid List.fold

  let pbyte c = elem ((=) c) (Some ("'" + (string c) + "'"))
  let bytes (Bin b) =
    takeWith (BinaryArray.length b) ((=) b) (Some (b.ToString()))

  let takeWhile1 p =
    takeWhile1 BinaryArray.monoid BinaryArray.span p

  let scan s p = scan BinaryArray.monoid BinaryArray.head BinaryArray.tail BinaryArray.take s p
  
  let parse p (Bin init) = parse BinaryArray.monoid p init

  let parseAll m init = parse (phrase m) init

  let cons m n = cons BinaryArray.cons m n

  let manySatisfy pred = many BinaryArray.monoid BinaryArray.cons (satisfy pred)

  let many p = many List.monoid List.cons p
  let many1 p = many1 List.monoid List.cons p

  let manyTill p q = manyTill List.monoid List.cons p q

  let sepBy1 p s = sepBy1 List.monoid List.cons p s
  let sepBy p s = sepBy List.monoid List.cons p s
