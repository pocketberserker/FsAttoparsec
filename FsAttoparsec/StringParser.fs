namespace Attoparsec

open System
open Helper

module String =

  let private monoid = BmpString.monoid

  module ParseResult =
    let feed s (result: ParseResult<_, _>) = ParseResult.feed monoid (BmpString.ofString s) result
    let done_ (result: ParseResult<_, _>) = ParseResult.done_ monoid result

  let parseOnly parser input =
    let input = BmpString.ofString input
    parseOnly monoid parser input

  let ensure n = ensure BmpString.length n

  let elem p what = elem BmpString.length BmpString.head BmpString.tail p what

  let satisfy p = satisfy BmpString.length BmpString.head BmpString.tail p

  let skip p what = skip BmpString.length BmpString.head BmpString.tail p what

  let skipWhile p = skipWhile monoid BmpString.skipWhile p

  let takeWith n p what = takeWith BmpString.length BmpString.splitAt n p what

  let take n = take BmpString.length BmpString.splitAt n

  let anyChar = satisfy (fun _ -> true)

  let notChar c = satisfy ((<>) c) |> as_ ("not '" + (string c) + "'")

  let takeWhile (p: _ -> bool) = takeWhile monoid BmpString.span p

  let takeRest = takeRest monoid

  let takeText = takeText monoid List.fold

  let char_ c = elem ((=) c) (Some ("'" + (string c) + "'"))
  
  let string_ (Bmp s) =
    takeWith (BmpString.length s) ((=) s) (Some ("\"" + (BmpString.toString s) + "\""))

  let stringTransform f (Bmp s) what =
    let what = match what with | Some s -> Some s | None -> Some "stringTransform(...)"
    takeWith (BmpString.length s) (fun x -> f x = f s) what

  let takeWhile1 p = takeWhile1 monoid BmpString.span p

  let private addDigit (a: decimal) c = a * 10M + ((decimal (int64  c)) - 48M)

  let decimal_ =
    takeWhile1 Char.IsDigit
    |>> (fun x -> x |> BmpString.fold addDigit 0M)

  let signedInt = char_ '-' >>. map (~-) decimal_ <|> (char_ '+' >>. decimal_) <|> decimal_

  let scientific = parser {
    let! positive = satisfy (fun c -> c = '-' || c = '+') |>> ((=) '+') <|> ok true
    let! n = decimal_
    let! s =
      (satisfy ((=) '.') >>. takeWhile Char.IsDigit
      |>> (fun f -> decimal ((string n) + "." + (BmpString.toString f))))
      <|> ok (decimal n)
    let sCoeff = if positive then s else -s
    return!
      satisfy (fun c -> c = 'e' || c = 'E')
      >>. signedInt
      >>= (fun x ->
        if int x > Int32.MaxValue then error ("Exponent too large: " + string s)
        else ok (s * (decimal (Math.Pow(10.0, float x))))) <|> ok sCoeff
  }

  let scan s p = scan monoid BmpString.head BmpString.tail BmpString.take s p

  let parse p (Bmp init) = parse monoid p init

  let parseAll m init = parse (phrase m) init
  
  let oneOf chars = satisfy (Helper.inClass chars)
  let noneOf chars = satisfy (Helper.inClass chars >> not)

  let alphaNum =
    satisfy (inClass "a-zA-Z")
    <|> satisfy Char.IsNumber
  let letter = satisfy Char.IsLetter

  let stringsSepBy p s =
    cons BmpString.append p ((s >>. sepBy1 BmpString.monoid BmpString.append p s) <|> ok BmpString.monoid.Mempty) <|> ok BmpString.monoid.Mempty
    |> as_ ("sepBy(" + p.ToString() + "," + s.ToString() + ")")

  let cons m n = cons List.cons m n

  let manySatisfy pred = many BmpString.monoid BmpString.cons (satisfy pred)

  let many p = many List.monoid List.cons p
  let many1 p = many1 List.monoid List.cons p

  let manyTill p q = manyTill List.monoid List.cons p q

  let sepBy1 p s = sepBy1 List.monoid List.cons p s
  let sepBy p s = sepBy List.monoid List.cons p s
