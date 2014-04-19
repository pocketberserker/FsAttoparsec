namespace Attoparsec

type State<'T> = {
  Input: 'T
  Added: 'T
  Complete: bool
  Monoid: Monoid<'T>
}
  with
    static member (+) (lhs, rhs) = {
      Input = lhs.Monoid.Mappend(lhs.Input, rhs.Added)
      Added = lhs.Monoid.Mappend(lhs.Added, rhs.Added)
      Complete = lhs.Complete || rhs.Complete
      Monoid = lhs.Monoid
    } 
    static member (+) (lhs, rhs) = {
      lhs with
        Input = lhs.Monoid.Mappend(lhs.Input, rhs)
        Added = lhs.Monoid.Mappend(lhs.Added, rhs)
    } 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
  let completed s = { s with Complete = true }
  let noAdds s = { s with Added = s.Monoid.Mempty}

module Internal =

  type Result<'T, 'U> =
    | Fail of input: 'T * stack: string list * message: string
    | Partial of ('T -> Result<'T, 'U>)
    | Done of input: 'T * result: 'U
    with
      member this.Translate =
        match this with
        | Fail(input, stack, message) -> ParseResult.Fail(input, stack, message)
        | Partial k -> ParseResult.Partial (fun a -> (k a).Translate)
        | Done(input, result) -> ParseResult.Done(input, result)

  module Result =
    let inline translate (result: Result<_, _>) = result.Translate
    let push s (Fail(input, stack, message)) = Fail(input, s :: stack, message)

type Failure<'T, 'U> = State<'T> * string list * string -> Internal.Result<'T, 'U>
type Success<'T, 'U, 'V> = State<'T> * 'U -> Internal.Result<'T, 'V>
  
[<AbstractClass>]
type Parser<'T, 'U> () =

  abstract member Apply: State<'T> * Failure<'T, 'V> * Success<'T, 'U, 'V> -> Internal.Result<'T, 'V>

  member this.Infix(s) = "(" + this.ToString() + ") " + s

  abstract member Bind: ('U -> Parser<'T, 'V>) -> Parser<'T, 'V>
  default this.Bind(f: 'U -> Parser<'T, 'V>) = { new Parser<'T, 'V>() with
    override x.ToString() =  this.Infix("bind ...")
    member x.Apply(st0, kf, ks) = this.Apply(st0, kf, (fun (s, a) -> (f a).Apply(s, kf, ks))) }

  abstract member Map: ('U -> 'V) -> Parser<'T, 'V>
  default this.Map(f: 'U -> 'V) = { new Parser<'T, 'V>() with
    override x.ToString() =  this.Infix("map ...")
    member x.Apply(st0, kf, ks) = this.Apply(st0, kf, (fun (s, a) -> ks (s, f a))) }

  abstract member Filter: ('U -> bool) -> Parser<'T, 'U>
  default this.Filter(pred: 'U -> bool) = { new Parser<'T, 'U>() with
    override x.ToString() =  this.Infix("filter ...")
    member x.Apply(st0, kf, ks) =
      this.Apply(st0, kf, (fun (s, a) -> if pred a then ks(s,a) else kf(s, [], "withFilter")))
    override x.Map(f) = this.Filter(pred).Map(f)
    override x.Bind(f) = { new Parser<'T, 'V>() with
      override y.ToString() = x.ToString()
      member y.Apply(st0, kf, ks) =
        this.Apply(st0, kf, fun (s, a) -> if pred a then (f a).Apply(s,kf,ks) else kf(s, [], "withFilter")) }
    override x.Filter(q) = this.Filter(fun y -> pred y && q y) }

  member this.As(s) = { new Parser<_, _>() with
    override x.ToString() = s
    member x.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> kf (st1, s :: stack, msg)
      this.Apply(st0, kf, ks) }

  member this.AsOpaque(s) = { new Parser<_, _>() with
    override x.ToString() = s
    member x.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> kf (st1, [], "Failure reading:" + s)
      this.Apply(st0, kf, ks) }

  member this.Parse(m:Monoid<_>, input) =
    let st = {
      Input = input
      Added = m.Mempty
      Complete = false
      Monoid = m
    }
    let kf =  fun (a, b, c) -> Internal.Fail(a.Input, b, c)
    let ks = fun (a, b) -> Internal.Done(a.Input, b)
    this.Apply(st, kf, ks)
    |> Internal.Result.translate

[<AutoOpen>]
module Combinator =

  open Internal

  let inline bind f (parser: Parser<_, _>) = parser.Bind(f)
  let inline map f (parser: Parser<_, _>) = parser.Map(f)
  let inline filter pred (parser: Parser<_, _>) = parser.Filter(pred)

  let ok a =
    { new Parser<_, _>() with
    override this.ToString() = "ok(" + a.ToString() + ")"
    member this.Apply(st0, kf, ks) = ks (st0, a) }
 
  let error what =
    { new Parser<_, _>() with
      override this.ToString() = "error(" + what + ")"
      member this.Apply(st0, kf, ks) = kf (st0, [], "Failed reading: " + what) }
  
  let zero<'T, 'U> : Parser<'T, 'U> = error "zero"

  let inline (>>=) p f = bind f p

  type ParserBuilder() =
    member this.Zero() = zero
    member this.Bind(x, f) = x >>= f
    member this.Return(x) = ok x
    member this.ReturnFrom(x) = x

  let parser = ParserBuilder()

  let parseOnly (m: Monoid<_>) (parser: Parser<_, _>) input =
    let state = { Input = input; Added = m.Mempty; Complete = true; Monoid = m }
    let kf =  fun (a, b, c) -> Fail(a.Input, b, c)
    let ks = fun (a, b) -> Done(a.Input, b)
    match parser.Apply(state, kf, ks) with
    | Fail(_, _, e) -> Choice2Of2 e
    | Done(_, a) -> Choice1Of2 a
    | _ -> failwith "parseOnly: Parser returned a partial result"

  let prompt st kf ks = Partial (fun s ->
    let m = st.Monoid
    if s = m.Mempty then kf { st with Complete = true }
    else ks { st with Input = m.Mappend(st.Input, s); Added = m.Mappend(st.Added, s) })

  let demandInput<'T when 'T : equality> = { new Parser<'T, unit>() with
    override this.ToString() = "demandInput"
    member this.Apply(st0, kf, ks) =
      if st0.Complete then
        kf(st0, ["demandInput"], "not enough bytes")
      else
        prompt st0 (fun st -> kf(st, ["demandInput"],"not enough bytes")) (fun a -> ks(a, ())) }

  let (>>.) (p: Parser<_, _>) (n: Parser<_, _>) = { new Parser<_, _>() with
    override this.ToString() = p.Infix(">>. " + n.ToString())
    member this.Apply(st0, kf, ks) = p.Apply(st0, kf, fun (s, a) -> n.Apply(s, kf, ks)) }
  
  let (.>>) (m: Parser<_, _>) (n: Parser<_, _>) = { new Parser<_, _>() with
    override this.ToString() = m.Infix(".>> " + n.ToString())
    member this.Apply(st0, kf, ks) =
      m.Apply(st0, kf, fun (st1, a) -> n.Apply(st1, kf, fun (st2, b) -> ks (st2, a))) }

  let rec ensure (length: 'T -> int) n = { new Parser<_, unit>() with
    override this.ToString() = "ensure(" + (string n) + ")"
    member this.Apply(st0, kf, ks) =
      if length st0.Input >= n then ks(st0,())
      else (demandInput >>. ensure length n).Apply(st0,kf,ks) }

  let wantInput<'T when 'T : equality> = { new Parser<'T, bool>() with
    override this.ToString() = "wantInput"
    member this.Apply(st0, kf, ks) =
      if st0.Input <> st0.Monoid.Mempty then ks(st0, true)
      elif st0.Complete then ks(st0, false)
      else prompt st0 (fun a -> ks(a, false)) (fun a -> ks(a, true)) }

  let atEnd<'T when 'T : equality> = wantInput<'T>.Map(not)

  let get<'T> = { new Parser<'T, _>() with
    override this.ToString() = "get"
    member this.Apply(st0, kf, ks) = ks(st0, st0.Input) }

  let put s = { new Parser<_, unit>() with
    override this.ToString() = "put(" + s.ToString() + ")"
    member this.Apply(st0, kf, ks) = ks({ st0 with Input = s }, ()) }

  let attempt (p: Parser<_, _>) = { new Parser<_, _>() with
    override this.ToString() = "attempt(" + p.ToString() + ")"
    member this.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> kf (st0 + st1, stack, msg)
      p.Apply(State.noAdds st0, kf, ks) }

  let elem length head tail p what =
    let what = match what with | Some s -> s | None -> "elem(...)"
    (ensure length 1 >>. get
    >>= (fun s ->
      let c = head s
      if p c then put (tail s) >>. ok c
      else error what
    )).AsOpaque(what)

  let satisfy length head tail p = elem length head tail p (Some "satisfy(...)")

  let skip length head tail p what =
    let what = match what with | Some s -> s | None -> "skip(...)"
    (ensure length 1 >>. get
    >>= (fun s ->
      if p (head s) then put (tail s)
      else error what
    )).AsOpaque(what)

  let rec skipWhile (m: Monoid<_>) dropWhile (p: _ -> bool) : Parser<_, unit> = parser {
    let! t = get |> map (dropWhile p)
    do! put t
    let! input =
      if t = m.Mempty then wantInput else zero
    return!
      if input then skipWhile m dropWhile p else zero
  }

  let takeWith length splitAt n p what =
    let what = match what with | Some s -> s | None -> "takeWith(...)"
    (ensure length n >>. get
    >>= (fun s ->
      let (w, h) = splitAt n s
      if p w then put h >>. ok w
      else error what
    )).AsOpaque(what)

  let take length splitAt n =
    takeWith length splitAt n (fun _ -> true) (Some ("take(" + (string n) + ")"))

  let takeWhile (m: Monoid<_>) span (p: _ -> bool) : Parser<_, _> =
    let rec inner acc = parser {
      let! x = get
      let (h, t) = span p x
      do! put t
      return!
        if m.Mempty = t then
          wantInput
          >>= (fun input ->
            if input then inner (h :: acc)
            else ok (h :: acc))
        else ok(h :: acc) }
    inner [] |> map (fun xs -> xs |> List.rev |> List.concat)

  let takeRest (m: Monoid<_>) =
    let rec inner acc = parser {
      let! input = wantInput
      return!
        if input then
          get >>= (fun s -> put m.Mempty >>= (fun () -> inner (s :: acc)))
        else ok (List.rev acc)
    }
    inner []

  let takeText (m: Monoid<_>) fold =
    takeRest m |> map (fold (fun a b -> m.Mappend(a, b)) m.Mempty)

  let private when' b (m: Parser<_, unit>) = if b then m else ok ()

  let takeWhile1 (m: Monoid<_>) span p = parser {
    do!
      get
      |> map (fun x -> x = m.Mempty)
      >>= (fun b -> when' b demandInput)
    let! s = get
    let (h, t) = span p s
    do! when' (List.isEmpty h) (error "takeWhile1")
    do! put t
    return!
      if t = m.Mempty then takeWhile m span p |> map (fun x -> List.append h x) else ok h
  }

  let endOfInput<'T when 'T : equality> = { new Parser<'T, unit>() with
    override this.ToString() = "endOfInput"
    member this.Apply(st0, kf, ks) =
      if (st0.Input = st0.Monoid.Mempty) then
        if st0.Complete then ks (st0, ())
        else
          let kf = fun (st1, stack, msg) -> ks (st0 + st1, ())
          let ks = fun (st1, u) -> kf (st0 + st1, [], "endOfInput")
          demandInput.Apply(st0, kf, ks)
      else kf (st0, [], "endOfInput")
  }
  
  let phrase p = (p .>> endOfInput).As("phrase" + p.ToString())
  
  let cons (m: Parser<_, _>) (n: Parser<_, _ list>) =
    m >>= (fun x -> n |> map (fun xs -> x :: xs))

  let (<|>) (m: Parser<_, _>) (n: Parser<_, _>) = { new Parser<_, _>() with
    override this.ToString() = m.Infix("<|> ...")
    member this.Apply(st0, kf, ks) =
      let kf = fun (st1, stack, msg) -> n.Apply(st0 + st1, kf, ks)
      m.Apply(State.noAdds st0, kf, ks)
  }

  module private Lazy =

    let cons m (n: Lazy<Parser<_, _ list>>) = m >>= (fun x -> n.Value |> map (fun xs -> x :: xs))
    let (>>.) (p: Parser<_, _>) (n: Lazy<Parser<_, _>>) = { new Parser<_, _>() with
      override this.ToString() = p.Infix(">>. " + n.ToString())
      member this.Apply(st0, kf, ks) = p.Apply(st0, kf, fun (s, a) -> n.Value.Apply(s, kf, ks)) }

  let many p =
    let rec manyP = lazy (Lazy.cons p manyP <|> ok [])
    manyP.Force().As("many(" + p.ToString() + ")")

  let many1 p = cons p (many p)

  type private Scan<'T, 'U> =
    | Continue of 'T
    | Finished of int * 'U

  let scan (m: Monoid<_>) head tail take s (p: _ -> _ -> Option<_>) =
    let rec scanner s n t =
      if t = m.Mempty then Continue s
      else
        match p s (head t) with
        | Some s -> scanner s (n + 1) (tail t)
        | None -> Finished(n, t)
    let rec inner acc s = parser {
      let! input = get
      return!
        match scanner s 0 input with
        | Continue sp ->
          put m.Mempty >>= (fun () ->
            wantInput
            >>= (fun more -> if more then inner (input :: acc) sp else ok (input :: acc)))
        | Finished(n, t) ->
          let i = input |> take n
          put t >>= (fun () -> ok (i :: acc))
    }
    parser {
      let! chunks = inner [] s
      return!
        match chunks with
        | [x] -> ok x
        | xs -> ok (xs |> List.rev |> List.fold (fun a b -> m.Mappend(a, b)) m.Mempty)
    }

  let manyTill p q =
    let rec scan = lazy (q >>. ok [] <|> Lazy.cons p scan)
    scan.Value.As("manyTill(" + p.ToString() + "," + q.ToString() + ")")
  
  let skipMany p =
    let rec scan = lazy (Lazy.(>>.) p scan <|> ok ())
    scan.Value.As("skipMany(" + p.ToString() + ")")

  let skipMany1 p = (p >>. skipMany p).As("skipMany1(" + p.ToString() + ")")

  let sepBy1 p s =
    let rec scan = lazy (cons p (Lazy.(>>.) s scan <|> ok []))
    scan.Value.As("sepBy1(" + p.ToString() + "," + s.ToString() + ")")

  let sepBy p s =
    (cons p ((s >>. sepBy1 p s) <|> ok []) <|> ok [])
      .As("sepBy(" + p.ToString() + "," + s.ToString() + ")")

  let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> ok (f' m')
  let inline ap m f = f <*> m
  let inline (<!>) f m = map f m
  let inline lift2 f a b = ok f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y

  let choice xs =
    (List.foldBack (<|>) xs (error "choice")).As("choice(" + xs.ToString() + " :_*)")

  let opt m = (attempt m |> map Some <|> ok None).As("opt(" + m.ToString() + ")")
