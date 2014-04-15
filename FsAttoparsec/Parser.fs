module Attoparsec

type ParseResult<'T> =
  | Fail of input: string * stack: string list * message: string
  | Partial of (string -> ParseResult<'T>)
  | Done of input: string * result: 'T
  with
    member this.Map(f) =
      match this with
      | Fail(input, stack, message) -> Fail(input, stack, message)
      | Partial k -> Partial(fun s -> (k s).Map(f))
      | Done(input, result) -> Done(input, f result)

    member this.Feed(s) =
      match this with
      | Fail _ -> this
      | Partial k -> k s
      | Done(input, result) -> Done(input + s, result)

    member this.Done_ =
      match this with
      | Fail _
      | Done _ -> this
      | Partial _ -> this.Feed("")

    member this.Option =
      match this with
      | Fail _
      | Partial _ -> None
      | Done(_, result) -> Some result

    member this.Choice =
      match this with
      | Fail(_, _, message) -> Choice2Of2 message
      | Partial _ -> Choice2Of2 "incomplete input"
      | Done(_, result) -> Choice1Of2 result

module ParseResult =

  let inline map f (result: ParseResult<_>) = result.Map(f)
  let inline feed s (result: ParseResult<_>) = result.Feed(s)
  let inline done_ (result: ParseResult<_>) = result.Done_
  let inline option (result: ParseResult<_>) = result.Option
  let inline choice (result: ParseResult<_>) = result.Choice

type State = {
  Input: string
  Added: string
  Complete: bool
}
  with
    static member (+) (lhs, rhs) = {
      Input = lhs.Input + rhs.Input
      Added = lhs.Added + rhs.Added
      Complete = lhs.Complete || rhs.Complete
    } 
    static member (+) (lhs, rhs) = {
      lhs with
        Input = lhs.Input + rhs
        Added = lhs.Added + rhs
    } 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
  let completed s = { s with Complete = true }
  let noAdds s = { s with Added = ""}
  let inline toString s = s.Input
  let inline ofString s = {
    Input = s
    Added = ""
    Complete = false
  }

module Internal =

  type Result<'T> =
    | Fail of input: string * stack: string list * message: string
    | Partial of (string -> Result<'T>)
    | Done of input: string * result: 'T
    with
      member this.Translate =
        match this with
        | Fail(input, stack, message) -> ParseResult.Fail(input, stack, message)
        | Partial k -> ParseResult.Partial (fun a -> (k a).Translate)
        | Done(input, result) -> ParseResult.Done(input, result)

  module Result =
    let inline translate (result: Result<_>) = result.Translate
    let push s (Fail(input, stack, message)) = Fail(input, s :: stack, message)

type Failure<'T> = State * string list * string -> Internal.Result<'T>
type Success<'T, 'U> = State * 'T -> Internal.Result<'U>
  
[<AbstractClass>]
type Parser<'T> () =

  abstract member Apply: State * Failure<'U> * Success<'T, 'U> -> Internal.Result<'U>

  member this.Infix(s) = "(" + this.ToString() + ") " + s

  abstract member Bind: ('T -> Parser<'U>) -> Parser<'U>
  default this.Bind(f: 'T -> Parser<'U>) = { new Parser<'U>() with
    override x.ToString() =  this.Infix("bind ...")
    member x.Apply(st0, kf, ks) = this.Apply(st0, kf, (fun (s, a) -> (f a).Apply(s, kf, ks))) }

  abstract member Map: ('T -> 'U) -> Parser<'U>
  default this.Map(f: 'T -> 'U) = { new Parser<'U>() with
    override x.ToString() =  this.Infix("map ...")
    member x.Apply(st0, kf, ks) = this.Apply(st0, kf, (fun (s, a) -> ks (s, f a))) }

  abstract member Filter: ('T -> bool) -> Parser<'T>
  default this.Filter(pred: 'T -> bool) = { new Parser<'T>() with
    override x.ToString() =  this.Infix("filter ...")
    member x.Apply(st0, kf, ks) =
      this.Apply(st0, kf, (fun (s, a) -> if pred a then ks(s,a) else kf(s, [], "withFilter")))
    override x.Map(f) = this.Filter(pred).Map(f)
    override x.Bind(f) = { new Parser<'U>() with
      override y.ToString() = x.ToString()
      member y.Apply(st0, kf, ks) =
        this.Apply(st0, kf, fun (s, a) -> if pred a then (f a).Apply(s,kf,ks) else kf(s, [], "withFilter")) }
    override x.Filter(q) = this.Filter(fun y -> pred y && q y) }

  member this.As(s) = { new Parser<_>() with
    override x.ToString() = s
    member x.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> kf (st1, s :: stack, msg)
      this.Apply(st0, kf, ks) }

  member this.AsOpaque(s) = { new Parser<_>() with
    override x.ToString() = s
    member x.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> kf (st1, [], "Failure reading:" + s)
      this.Apply(st0, kf, ks) }

  member this.Parse(input) =
    let st = State.ofString input
    let kf =  fun (a, b, c) -> Internal.Fail(State.toString a, b, c)
    let ks = fun (a, b) -> Internal.Done(State.toString a, b)
    this.Apply(st, kf, ks)
    |> Internal.Result.translate

module Parser =

  let inline bind f (parser: Parser<_>) = parser.Bind(f)
  let inline map f (parser: Parser<_>) = parser.Map(f)
  let inline filter pred (parser: Parser<_>) = parser.Filter(pred)

  let inline (>>=) p f = bind f p

  open Internal

  let parseOnly input (parser: Parser<_>) =
    let state = { Input = input; Added = ""; Complete = true }
    let kf =  fun (a, b, c) -> Fail(State.toString a, b, c)
    let ks = fun (a, b) -> Done(State.toString a, b)
    match parser.Apply(state, kf, ks) with
    | Fail(_, _, e) -> Choice2Of2 e
    | Done(_, a) -> Choice1Of2 a
    | _ -> failwith "parseOnly: Parser returned a partial result"

  let ok a =
    { new Parser<_>() with
    override this.ToString() = "ok(" + a.ToString() + ")"
    member this.Apply(st0, kf, ks) = ks (st0, a) }

  let error what =
    { new Parser<_>() with
      override this.ToString() = "err(" + what + ")"
      member this.Apply(st0, kf, ks) = kf (st0, [], "Failed reading: " + what) }

  let prompt st kf ks = Partial (fun s ->
    if s = "" then kf { st with Complete = true }
    else ks (st + s))

  let demandInput = { new Parser<unit>() with
    override this.ToString() = "demandInput"
    member this.Apply(st0, kf, ks) =
      if st0.Complete then
        kf(st0, ["demandInput"], "not enough bytes")
      else
        prompt st0 (fun st -> kf(st, ["demandInput"],"not enough bytes")) (fun a -> ks(a, ())) }

  let (>>.) (p: Parser<_>) (n: Parser<_>) = { new Parser<_>() with
    override this.ToString() = p.Infix(">>. " + n.ToString())
    member this.Apply(st0, kf, ks) = p.Apply(st0, kf, fun (s, a) -> n.Apply(s, kf, ks)) }
  
  let (.>>) (m: Parser<_>) (n: Parser<_>) = { new Parser<_>() with
    override this.ToString() = m.Infix(".>> " + n.ToString())
    member this.Apply(st0, kf, ks) =
      m.Apply(st0, kf, fun (st1, a) -> n.Apply(st1, kf, fun (st2, b) -> ks (st2, a))) }

  let rec ensure n = { new Parser<unit>() with
    override this.ToString() = "ensure(" + (string n) + ")"
    member this.Apply(st0, kf, ks) =
      if String.length st0.Input >= n then ks(st0,())
      else (demandInput >>. ensure n).Apply(st0,kf,ks) }

  let wantInput = { new Parser<bool>() with
    override this.ToString() = "wantInput"
    member this.Apply(st0, kf, ks) =
      if st0.Input <> "" then ks(st0, true)
      elif st0.Complete then ks(st0, false)
      else prompt st0 (fun a -> ks(a, false)) (fun a -> ks(a, true)) }

  let atEnd = wantInput.Map(not)

  let get = { new Parser<string>() with
    override this.ToString() = "get"
    member this.Apply(st0, kf, ks) = ks(st0, st0.Input) }

  let put s = { new Parser<unit>() with
    override this.ToString() = "put(" + s + ")"
    member this.Apply(st0, kf, ks) = ks({ st0 with Input = s }, ()) }

  let attempt (p: Parser<_>) = { new Parser<_>() with
    override this.ToString() = "attempt(" + p.ToString() + ")"
    member this.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> kf (st0 + st1, stack, msg)
      p.Apply(State.noAdds st0, kf, ks) }

  let elem p what =
    let what = match what with | Some s -> s | None -> "elem(...)"
    (ensure 1 >>. get
    >>= (fun s ->
      let c = s.Chars(0)
      if p c then put(s.Substring(1)) >>. ok c
      else error what
    )).AsOpaque(what)

  let satisfy p = elem p (Some "satisfy(...)")

  let skip s p what =
    let what = match what with | Some s -> s | None -> "skip(...)"
    (ensure 1 >>. get
    >>= (fun s ->
      if p (s.Chars(0)) then put (s.Substring(1))
      else error what
    )).AsOpaque(what)

  let takeWith n p what =
    let what = match what with | Some s -> s | None -> "takeWith(...)"
    (ensure n >>. get
    >>= (fun s ->
      let w = s.Substring(0,n)
      if p w then put (s.Substring(n)) >>. ok w
      else error what
    )).AsOpaque(what)

  let take n = takeWith n (fun _ -> true) (Some ("take(" + (string n) + ")"))

  let anyChar: Parser<char> = satisfy (fun _ -> true)

  let notChar c = (satisfy ((<>) c)).As("not '" + (string c) + "'")

  type ParserBuilder() =
    member this.Bind(x, f) = x >>= f
    member this.Return(x) = ok x
    member this.ReturnFrom(x) = x

  let parser = ParserBuilder()

  let takeWhile p =
    let rec inner acc = parser {
      let! x = get
      let (h, t) = List.partition p (List.ofSeq x)
      let! _ = put (System.String (t |> List.toArray))
      return!
        if List.isEmpty t then
          wantInput
          >>= (fun input ->
            if input then inner (h :: acc)
            else ok (h :: acc))
        else ok(h :: acc) }
    inner [] |> map (fun xs -> System.String(xs |> List.rev |> List.concat |> List.toArray))

  let takeRest =
    let rec inner acc = parser {
      let! input = wantInput
      return!
        if input then
          get
          >>= (fun s ->
            put("") >>= (fun _ -> inner (s :: acc)))
        else ok (List.rev acc)
    }
    inner []

  let takeText = takeRest |> map (List.fold (+) "")

  let private when' b (m: Parser<unit>) = if b then m else ok ()

  let takeWhile1 p : Parser<string> = parser {
    let! _ =
      get
      |> map (System.String.IsNullOrEmpty)
      >>= (fun b -> when' b demandInput)
    let! s = get
    let (h, t) = List.partition p (List.ofSeq s)
    let! _ = when' (List.isEmpty h) (error "takeWhile1")
    let! _ = put (System.String(Array.ofList t))
    let h =  System.String(Array.ofList h)
    return!
      if List.isEmpty t then takeWhile p |> map (fun x -> h + x) else ok h
  }

  let char_ c = elem ((=) c) (Some ("'" + (string c) + "'"))
  let string_ s = takeWith (String.length s) ((=) s) (Some ("\"" + s + "\""))

  let stringTransform f s what =
    let what = match what with | Some s -> Some s | None -> Some "stringTransform(...)"
    takeWith (String.length s) (fun x -> f x = f s) what

  let endOfInput = { new Parser<unit>() with
    override this.ToString() = "endOfInput"
    member this.Apply(st0, kf, ks) =
      if (st0.Input = "") then
        if st0.Complete then ks (st0, ())
        else
          let kf = fun (st1, stack, msg) -> ks (st0 + st1, ())
          let ks = fun (st1, u) -> kf (st0 + st1, [], "endOfInput")
          demandInput.Apply(st0, kf, ks)
      else kf (st0, [],"endOfInput")
  }

  let phrase (p: Parser<_>) = (p .>> endOfInput).As("phrase" + p.ToString())

  let private addDigit (a: decimal) (c: char) = a * 10M + ((decimal (int64  c)) - 48M)

  let decimal_ = takeWhile1 (System.Char.IsDigit) |> map (fun x -> x |> Seq.fold addDigit 0M)

  let cons (m: Parser<_>) (n: Parser<_ list>) = m >>= (fun x -> n |> map (fun xs -> x :: xs))

  // TODO: modify operator name
  let (<|>) (m: Parser<_>) (n: Parser<_>) = { new Parser<_>() with
    override this.ToString() = m.Infix("<|> ...")
    member this.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> n.Apply(st0 + st1, kf, ks)
      m.Apply(State.noAdds st0, kf, ks)
  }

  let signedInt = char_ '-' >>. map (~-) decimal_ <|> (char_ '+' >>. decimal_) <|> decimal_

  let scientific = parser {
    let! positive = satisfy (fun c -> c = '-' || c = '+') |> map ((=) '+') <|> ok true
    let! n = decimal_
    let! s =
      (satisfy ((=) '.') >>. takeWhile (System.Char.IsDigit) |> map (fun f -> decimal ((string n) + "." + f))) <|> ok (decimal n)
    let sCoeff = if positive then s else -s
    return!
      satisfy (fun c -> c = 'e' || c = 'E')
      >>. signedInt.Bind(fun x ->
        if int x > System.Int32.MaxValue then error ("Exponent too large: " + string s)
        else ok (s * (decimal (System.Math.Pow(10.0, float x)))))
         <|> ok sCoeff
  }

  let many (p: Parser<_>) =
    let rec manyP = lazy (cons p (manyP.Force()) <|> ok [])
    manyP.Force().As("many(" + p.ToString() + ")")

  let many1 (p: Parser<_>) = cons p (many p)

  type private Scan<'T> =
    | Continue of 'T
    | Finished of int * string

  let scan s (p: _ -> char -> Option<_>) =
    let rec scanner s n t =
      match List.ofSeq t with
      | [] -> Continue s
      | x::xs ->
        match p s x with
        | Some s -> scanner s (n + 1) xs
        | None -> Finished(n, System.String(Array.ofList t))
    let rec inner acc s = parser {
      let! input = get
      return!
        match scanner s 0 (List.ofSeq input) with
        | Continue sp ->
          put("") >>= (fun _ -> wantInput >>= (fun more -> if more then inner (input :: acc) sp else ok (input :: acc)))
        | Finished(n, t) ->
          let i = System.String(input |> Seq.take n |> Seq.toArray)
          put t >>= (fun _ -> ok (i :: acc))
    }
    parser {
      let! chunks = inner [] s
      return!
        match chunks with
        | [x] -> ok(x)
        | xs -> ok (xs |> List.rev |> List.fold (+) "")
    }

  let manyTill (p: Parser<_>) (q: Parser<_>) =
    let rec scan = lazy (q >>. ok [] <|> cons p (scan.Force()))
    scan.Force().As("manyTill(" + p.ToString() + "," + q.ToString() + ")")
  
  let skipMany (p: Parser<_>) =
    let rec scan = lazy (p >>. (scan.Force()) <|> ok ())
    scan.Force().As("skipMany(" + p.ToString() + ")")

  let skipMany1 (p: Parser<_>) = (p >>. skipMany p).As("skipMany1(" + p.ToString() + ")")

  let sepBy1 (p: Parser<_>) (s: Parser<_>) =
    let rec scan = lazy (cons p (s >>. (scan.Force()) <|> ok []))
    scan.Force().As("sepBy1(" + p.ToString() + "," + s.ToString() + ")")

  let sepBy (p: Parser<_>) (s: Parser<_>) =
    (cons p (s >>. sepBy1 p s) <|> ok []) <|> (ok []).As("sepBy(" + p.ToString() + "," + s.ToString() + ")")

  let inline parse (m: Parser<_>) init = m.Parse(init)

  let parseAll (m: Parser<_>) init = parse (phrase m) init

module Helper =

  open System.Text.RegularExpressions

  let isMatch regex item = Regex.IsMatch(item, regex)
