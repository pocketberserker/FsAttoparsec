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
    parseOnly BmpString.skip monoid parser input

  let get = get BmpString.skip

  let endOfChunk = endOfChunk BmpString.length

  let wantInput = wantInput BmpString.length

  let atEnd = atEnd BmpString.length

  let ensureSuspended st n = ensureSuspended BmpString.length BmpString.substring st n

  let ensure n = ensure BmpString.length BmpString.substring n

  let elem p what = elem BmpString.length BmpString.head BmpString.substring p what

  let satisfy p = satisfy BmpString.length BmpString.head BmpString.substring p

  let skip p what = skip BmpString.length BmpString.head BmpString.substring p what

  let skipWhile p = skipWhile monoid BmpString.skipWhile p

  let takeWith n p what = takeWith BmpString.length BmpString.substring n p what

  let take n = take BmpString.length BmpString.substring n

  let anyChar = satisfy (fun _ -> true)

  let notChar c = satisfy ((<>) c) |> as_ ("not '" + (string c) + "'")

  let takeWhile (p: _ -> bool) = takeWhile monoid BmpString.takeWhile BmpString.length BmpString.skip p

  let takeRest = takeRest monoid BmpString.length BmpString.skip

  let takeText = takeText monoid BmpString.length BmpString.skip List.fold

  let pchar c = elem ((=) c) (Some ("'" + (string c) + "'"))
  
  let pstring (Bmp s) =
    takeWith (BmpString.length s) ((=) s) (Some ("\"" + (BmpString.toString s) + "\""))

  let stringTransform f (Bmp s) what =
    let what = match what with | Some s -> Some s | None -> Some "stringTransform(...)"
    takeWith (BmpString.length s) (fun x -> f x = f s) what

  let takeWhile1 p = takeWhile1 monoid BmpString.takeWhile BmpString.length BmpString.skip p

  let private addDigit (a: decimal) c = a * 10M + ((decimal (int64  c)) - 48M)

  let pdecimal = takeWhile1 Char.IsDigit |>> BmpString.fold addDigit 0M

  let signedInt = pchar '-' >>. map (~-) pdecimal <|> (pchar '+' >>. pdecimal) <|> pdecimal

  let scientific = parser {
    let! positive = satisfy (fun c -> c = '-' || c = '+') |>> ((=) '+') <|> ok true
    let! n = pdecimal
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

  let scan s p = scan monoid BmpString.head BmpString.tail BmpString.take BmpString.length BmpString.skip s p

  let parse p (Bmp init) = parse BmpString.skip monoid p init

  let endOfInput = endOfInput BmpString.length

  let phrase m = phrase BmpString.length m

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

  let newline = manySatisfy (fun i -> inClass "\r\n" i || inClass "\r" i || inClass "\n" i);
  let spaces = manySatisfy (fun i -> inClass "\r\n" i || inClass "\r" i || inClass "\n" i || Char.IsWhiteSpace i)

  let match_ p = match_ BmpString.substring p
