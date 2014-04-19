namespace Attoparsec

module String =

  let private monoid = { new Monoid<string>() with
    member this.Mempty = ""
    member this.Mappend(s, t) = s + t
  }

  module ParseResult =
    let feed s (result: ParseResult<_, _>) = ParseResult.feed monoid s result
    let done_ (result: ParseResult<_, _>) = ParseResult.done_ monoid result

  let parseOnly parser input = parseOnly monoid parser input

  let ensure n = ensure String.length n

  let inline private length s = String.length s
  let inline private head (s: string) = s.Chars(0)
  let inline private tail (s: string) = s.Substring(1)
  let private splitAt n (s: string) = (s.Substring(0, n), s.Substring(n))

  let elem p what = elem length head tail p what

  let satisfy p = satisfy length head tail p

  let skip p what = skip length head tail p what

  let takeWith n p what = takeWith length splitAt n p what

  let take n = take length splitAt n

  let anyChar: Parser<string, char> = satisfy (fun _ -> true)

  let notChar c = (satisfy ((<>) c)).As("not '" + (string c) + "'")

  let private span pred (s: string) =
    let l = (s |> List.ofSeq)
    let t = l |> Seq.takeWhile pred |> Seq.toList
    let u: string = System.String(l |> Seq.skipWhile pred |> Seq.toArray)
    (t, u)

  let takeWhile (p: _ -> bool) : Parser<string, string> =
    takeWhile monoid span p
    |> map (fun xs -> System.String(Array.ofList xs))

  let takeRest = takeRest monoid

  let takeText = takeText monoid List.fold

  let char_ c = elem ((=) c) (Some ("'" + (string c) + "'"))
  let string_ s = takeWith (String.length s) ((=) s) (Some ("\"" + s + "\""))

  let stringTransform f s what =
    let what = match what with | Some s -> Some s | None -> Some "stringTransform(...)"
    takeWith (String.length s) (fun x -> f x = f s) what

  let takeWhile1 p : Parser<string, string> =
    takeWhile1 monoid span p
    |> map (fun xs -> System.String(Array.ofList xs))

  let private addDigit (a: decimal) (c: char) = a * 10M + ((decimal (int64  c)) - 48M)

  let decimal_ =
    takeWhile1 System.Char.IsDigit
    |> map (fun x -> x |> Seq.fold addDigit 0M)

  let signedInt = char_ '-' >>. map (~-) decimal_ <|> (char_ '+' >>. decimal_) <|> decimal_

  let scientific = parser {
    let! positive = satisfy (fun c -> c = '-' || c = '+') |> map ((=) '+') <|> ok true
    let! n = decimal_
    let! s =
      (satisfy ((=) '.') >>. takeWhile (System.Char.IsDigit) |> map (fun f -> decimal ((string n) + "." + f)))
      <|> ok (decimal n)
    let sCoeff = if positive then s else -s
    return!
      satisfy (fun c -> c = 'e' || c = 'E')
      >>. signedInt.Bind(fun x ->
        if int x > System.Int32.MaxValue then error ("Exponent too large: " + string s)
        else ok (s * (decimal (System.Math.Pow(10.0, float x))))) <|> ok sCoeff
  }

  let scan s p = scan monoid head tail (fun n (x: string) -> x.Substring(n)) s p

  let parse (m: Parser<string, _>) init = m.Parse(monoid, init)

  let parseAll m init = parse (phrase m) init