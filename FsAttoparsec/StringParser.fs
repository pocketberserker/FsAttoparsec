namespace Attoparsec

module String =

  let private monoid = BmpString.monoid

  module ParseResult =
    let feed s (result: ParseResult<_, _>) = ParseResult.feed monoid (BmpString.ofString s) result
    let done_ (result: ParseResult<_, _>) = ParseResult.done_ monoid result

  let parseOnly parser input =
    let input = BmpString.ofString input
    parseOnly monoid parser input

  let ensure n = ensure String.length n

  let elem p what = elem BmpString.length BmpString.head BmpString.tail p what

  let satisfy p = satisfy BmpString.length BmpString.head BmpString.tail p

  let skip p what = skip BmpString.length BmpString.head BmpString.tail p what

  let skipWhile p = skipWhile monoid BmpString.skipWhile p

  let takeWith n p what = takeWith BmpString.length BmpString.splitAt n p what

  let take n = take BmpString.length BmpString.splitAt n

  let anyChar = satisfy (fun _ -> true)

  let notChar c = (satisfy ((<>) c)).As("not '" + (string c) + "'")

  let takeWhile (p: _ -> bool) = takeWhile monoid BmpString.span p

  let takeRest = takeRest monoid

  let takeText = takeText monoid List.fold

  let char_ c = elem ((=) c) (Some ("'" + (string c) + "'"))
  let string_ s = takeWith (BmpString.length s) ((=) s) (Some ("\"" + (BmpString.toString s) + "\""))

  let stringTransform f s what =
    let what = match what with | Some s -> Some s | None -> Some "stringTransform(...)"
    takeWith (String.length s) (fun x -> f x = f (BmpString.ofString s)) what

  let takeWhile1 p = takeWhile1 monoid BmpString.span p

  let private addDigit (a: decimal) c = a * 10M + ((decimal (int64  c)) - 48M)

  let decimal_ =
    takeWhile1 (char >> System.Char.IsDigit)
    |> map (fun x -> x |> BmpString.fold addDigit 0M)

  let signedInt = char_ '-' >>. map (~-) decimal_ <|> (char_ '+' >>. decimal_) <|> decimal_

  let scientific = parser {
    let! positive = satisfy (fun c -> c = '-' || c = '+') |> map ((=) '+') <|> ok true
    let! n = decimal_
    let! s =
      (satisfy ((=) '.') >>. takeWhile (char >> System.Char.IsDigit)
      |> map (fun f -> decimal ((string n) + "." + (BmpString.toString f))))
      <|> ok (decimal n)
    let sCoeff = if positive then s else -s
    return!
      satisfy (fun c -> c = 'e' || c = 'E')
      >>. signedInt.Bind(fun x ->
        if int x > System.Int32.MaxValue then error ("Exponent too large: " + string s)
        else ok (s * (decimal (System.Math.Pow(10.0, float x))))) <|> ok sCoeff
  }

  let scan s p = scan monoid BmpString.head BmpString.tail BmpString.skip s p

  let parse (m: Parser<_, _>) init =
    let init = BmpString.ofString init
    m.Parse(monoid, init)

  let parseAll m init = parse (phrase m) init
  
  let oneOf chars = satisfy (char >> Helper.inClass chars)
  let noneOf chars = satisfy (char >> Helper.inClass chars >> not)

  let alphaNum =
    satisfy (char >> Helper.inClass "a-zA-Z")
    <|> satisfy (char >> System.Char.IsNumber)
  let letter = satisfy (char >> System.Char.IsLetter)
