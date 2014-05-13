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

module State =
  let completed s = { s with Complete = true }
  let noAdds s = { s with Added = s.Monoid.Mempty}

module Internal =

  type Result<'T, 'U> =
    | Fail of 'T * string list * string
    | Partial of ('T -> ITrampoline<Result<'T, 'U>>)
    | Done of 'T * 'U
    with
      member this.Translate =
        match this with
        | Fail(input, stack, message) -> ParseResult.Fail(input, stack, message)
        | Partial k -> ParseResult.Partial (fun a -> (Trampoline.run (k a)).Translate)
        | Done(input, result) -> ParseResult.Done(input, result)

  module Result =
    let inline translate (result: Result<_, _>) = result.Translate

type Failure<'T, 'U> = State<'T> * string list * string -> ITrampoline<Internal.Result<'T, 'U>>
type Success<'T, 'U, 'V> = State<'T> * 'U -> ITrampoline<Internal.Result<'T, 'V>>
  
type Parser<'T, 'U> =
  abstract member Apply: State<'T> * Failure<'T, 'V> * Success<'T, 'U, 'V> -> ITrampoline<Internal.Result<'T, 'V>>

module private IParser =
  let infix s p = "(" + p.ToString() + ") " + s

type private BindP<'T, 'U, 'V>(p: Parser<'T, 'U>, f: 'U -> Parser<'T, 'V>) =
  override this.ToString() = IParser.infix "bind ..." p
  with
    interface Parser<'T, 'V> with
      member this.Apply(st0, kf, ks) =
        let ks = fun (s, a) -> (f a).Apply(s, kf, ks)
        DelayT<_>(fun () -> p.Apply(st0, kf, ks)) :> _

type private MapP<'T, 'U, 'V>(p: Parser<'T, 'U>, f: 'U -> 'V) =
  override x.ToString() =  IParser.infix "map ..." p
  with
    interface Parser<'T, 'V> with
      member x.Apply(st0, kf, ks) =
        DelayT<_>(fun () -> p.Apply(st0, kf, (fun (s, a) -> DelayT<_>(fun () -> ks (s, f a)) :> _))) :> _

type private FilterP<'T, 'U>(p: Parser<'T, 'U>, pred: 'U -> bool) =
  override x.ToString() =  IParser.infix "filter ..." p
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) =
        p.Apply(st0, kf, (fun (s, a) -> if pred a then ks (s, a) else kf(s, [], "withFilter")))

type private AsP<'T, 'U>(p: Parser<'T, 'U>, s: string) =
  override x.ToString() = s
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) =
        let kf = fun (st1, stack, msg) -> kf (st1, s :: stack, msg)
        p.Apply(st0, kf, ks)

type private AsOpaqueP<'T, 'U>(p: Parser<'T, 'U>, s: string) =
  override x.ToString() = s
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) =
        let kf = fun (st1, stack, msg) -> kf (st1, [], "Failure reading:" + s)
        p.Apply(st0, kf, ks)

type private ReturnP<'T, 'U>(a: 'U) =
  override x.ToString() =
    match box a with
    | null -> "ok(unit or null)"
    | _ -> "ok(" + a.ToString() + ")"
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) = ks (st0, a)

type private ErrorP<'T, 'U>(what: string) =
  override x.ToString() = "error(" + what + ")"
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) = kf (st0, [], "Failed reading: " + what)

[<AutoOpen>]
module Parser =

  open Internal

  let infix s p = IParser.infix s p

  let parse (m: Monoid<_>) (p: Parser<_, _>) input =
    let st = {
      Input = input
      Added = m.Mempty
      Complete = false
      Monoid = m
    }
    let kf = fun (a, b, c) -> ReturnT<_>(Internal.Fail(a.Input, b, c)) :> ITrampoline<_>
    let ks = fun (a, b) -> ReturnT<_>(Internal.Done(a.Input, b)) :> ITrampoline<_>
    p.Apply(st, kf, ks)
    |> Trampoline.run
    |> Internal.Result.translate

  let bind f p = BindP<_, _, _>(p, f) :> Parser<_, _>
  let map f p = MapP<_, _, _>(p, f) :> Parser<_, _>
  let filter pred p = FilterP<_, _>(p, pred) :> Parser<_, _>
  let as_ s p = AsP<_, _>(p, s) :> Parser<_, _>
  let asOpaque s p = AsOpaqueP<_, _>(p, s) :> Parser<_, _>

  let ok a = ReturnP<_, _>(a) :> Parser<_, _>
 
  let error what = ErrorP<_, _>(what) :> Parser<_, _>
  
  let zero<'T, 'U> : Parser<'T, 'U> = error "zero"

  let inline (>>=) p f = bind f p
  let inline (|>>) p f = map f p
  let inline (>>%) p x = p |>> (fun _ -> x)

  type ParserBuilder() =
    member this.Zero() = zero
    member this.Bind(x, f) = x >>= f
    member this.Return(x) = ok x
    member this.ReturnFrom(x) = x

  let parser = ParserBuilder()

  let parseOnly (m: Monoid<_>) (parser: Parser<_, _>) input =
    let state = { Input = input; Added = m.Mempty; Complete = true; Monoid = m }
    let kf = fun (a, b, c) -> ReturnT<_>(Internal.Fail(a.Input, b, c)) :> ITrampoline<_>
    let ks = fun (a, b) -> ReturnT<_>(Internal.Done(a.Input, b)) :> ITrampoline<_>
    match Trampoline.run <| parser.Apply(state, kf, ks) with
    | Fail(_, _, e) -> Choice2Of2 e
    | Done(_, a) -> Choice1Of2 a
    | _ -> failwith "parseOnly: Parser returned a partial result"

  let prompt st kf ks = Partial (fun s ->
    let m = st.Monoid
    if s = m.Mempty then DelayT<_>(fun () -> kf { st with Complete = true }) :> ITrampoline<_>
    else
      DelayT<_>(fun () -> ks { st with Input = m.Mappend(st.Input, s); Added = m.Mappend(st.Added, s) })
      :> ITrampoline<_>)

  type private DemandInputP<'T when 'T : equality>() =
    override this.ToString() = "demandInput"
    with
      interface Parser<'T, unit> with
        member this.Apply(st0, kf, ks) =
          if st0.Complete then
            DelayT<_>(fun () -> kf(st0, ["demandInput"], "not enough bytes")) :> _
          else
            ReturnT<_>(prompt st0 (fun st -> kf(st, ["demandInput"],"not enough bytes")) (fun a -> ks(a, ()))) :> _

  let inline private demandInput'<'T when 'T : equality> () = DemandInputP<'T>() :> Parser<'T, unit>
  let demandInput<'T when 'T : equality> = demandInput'<'T> ()

  type private RightP<'T, 'U, 'V>(p: Parser<'T, 'U>, n: Parser<'T, 'V>) =
    override this.ToString() = IParser.infix (">>. " + n.ToString()) p
    with
      interface Parser<'T, 'V> with
        member this.Apply(st0, kf, ks) = p.Apply(st0, kf, fun (s, a) -> n.Apply(s, kf, ks))

  let (>>.) (p: Parser<_, _>) (n: Parser<_, _>) = RightP<_, _, _>(p, n) :> Parser<_, _>

  type private LeftP<'T, 'U, 'V>(m: Parser<'T, 'U>, n: Parser<'T, 'V>) =
    override this.ToString() = IParser.infix (".>> " + n.ToString()) m
    with
      interface Parser<'T, 'U> with
        member this.Apply(st0, kf, ks) = m.Apply(st0, kf, fun (st1, a) -> n.Apply(st1, kf, fun (st2, b) -> ks (st2, a)))
  
  let (.>>) (m: Parser<_, _>) (n: Parser<_, _>) = LeftP<_, _, _>(m, n) :> Parser<_, _>

  type private EnsureP<'T when 'T : equality>(length: 'T -> int, n: int) =
    override this.ToString() = "ensure(" + (string n) + ")"
    with
      interface Parser<'T, unit> with
        member this.Apply(st0, kf, ks) =
          if length st0.Input >= n then ks(st0, ())
          else (demandInput >>. EnsureP<'T>(length, n)).Apply(st0, kf, ks)

  let ensure (length: 'T -> int) n = EnsureP<_>(length, n) :> Parser<_, unit>

  type private WantInputP<'T when 'T : equality>() =
    override this.ToString() = "wantInput"
    with
      interface Parser<'T, bool> with
        member this.Apply(st0, kf, ks) =
          if st0.Input <> st0.Monoid.Mempty then ks (st0, true)
          elif st0.Complete then ks (st0, false)
          else ReturnT<_>(prompt st0 (fun a -> ks(a, false)) (fun a -> ks(a, true))) :> _

  let inline private wantInput'<'T when 'T : equality> () = WantInputP<_>() :> Parser<'T, bool>
  let wantInput<'T when 'T : equality> = wantInput'<'T> ()

  let atEnd<'T when 'T : equality> = wantInput<'T> |>> not

  type private GetP<'T>() =
    override this.ToString() = "get"
    with
      interface Parser<'T, 'T> with
        member this.Apply(st0, kf, ks) = ks(st0, st0.Input)

  let inline private get'<'T> () = GetP<'T>() :> Parser<'T, _>
  let get<'T> = get'<'T> ()

  type private PutP<'T>(s) =
    override this.ToString() = "put(" + s.ToString() + ")"
    with
      interface Parser<'T, unit> with
        member this.Apply(st0, kf, ks) = ks({ st0 with Input = s }, ())

  let put s = PutP<_>(s) :> Parser<_, unit>

  type private AttemptP<'T, 'U>(p: Parser<'T, 'U>) =
    override this.ToString() = "attempt(" + p.ToString() + ")"
    with
      interface Parser<'T, 'U> with
        member this.Apply(st0, kf, ks) =
          let kf = fun (st1, stack, msg) -> kf (st0 + st1, stack, msg)
          p.Apply(State.noAdds st0, kf, ks)

  let attempt p = AttemptP<_, _>(p) :> Parser<_, _>

  let elem length head tail p what =
    let what = match what with | Some s -> s | None -> "elem(...)"
    (ensure length 1 >>. get
    >>= (fun s ->
      let c = head s
      if p c then put (tail s) >>. ok c
      else error what
    ))
    |> asOpaque what

  let satisfy length head tail p = elem length head tail p (Some "satisfy(...)")

  let skip length head tail p what =
    let what = match what with | Some s -> s | None -> "skip(...)"
    (ensure length 1 >>. get
    >>= (fun s ->
      if p (head s) then put (tail s)
      else error what
    ))
    |> asOpaque what

  let rec skipWhile (m: Monoid<_>) dropWhile (p: _ -> bool) : Parser<_, unit> = parser {
    let! t = get |>> (dropWhile p)
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
    ))
    |> asOpaque what

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
            if input then inner (m.Mappend(h, acc))
            else ok (m.Mappend(h, acc)))
        else ok(m.Mappend(h, acc)) }
    inner m.Mempty

  let takeRest (m: Monoid<_>) =
    let rec inner acc = parser {
      let! input = wantInput
      return!
        if input then get >>= (fun s -> put m.Mempty >>. inner (s :: acc))
        else ok (List.rev acc)
    }
    inner []

  let takeText (m: Monoid<_>) fold =
    takeRest m |>> (fold (fun a b -> m.Mappend(a, b)) m.Mempty)

  let private when' (m: Parser<_, unit>) b = if b then m else ok ()

  let takeWhile1 (m: Monoid<_>) span p = parser {
    do! get |>> ((=) m.Mempty) >>= when' demandInput
    let! s = get
    let (h, t) = span p s
    do! when' (error "takeWhile1") (m.Mempty = h) 
    do! put t
    return!
      if t = m.Mempty then takeWhile m span p |>> (fun x -> m.Mappend(h, x)) else ok h
  }

  type private EndOfInputP<'T when 'T : equality>() =
    override this.ToString() = "endOfInput"
    with
      interface Parser<'T, unit> with
        member this.Apply(st0, kf, ks) =
          if (st0.Input = st0.Monoid.Mempty) then
            if st0.Complete then ks (st0, ())
            else
              let kf = fun (st1, stack, msg) -> ks (st0 + st1, ())
              let ks = fun (st1, u) -> kf (st0 + st1, [], "endOfInput")
              demandInput.Apply(st0, kf, ks)
          else kf (st0, [], "endOfInput")

  let inline private endOfInput'<'T when 'T : equality> () = EndOfInputP<_>() :> Parser<'T, unit>
  let endOfInput<'T when 'T : equality> = endOfInput'<'T> ()
  
  let phrase p = p .>> endOfInput |> as_ ("phrase" + p.ToString())
  
  let cons m n = m >>= (fun x -> n |>> (fun xs -> x :: xs))

  type private OrP<'T, 'U>(m: Parser<'T, 'U>, n: Parser<'T, 'U>) =
    override this.ToString() = IParser.infix ("<|> ...") m
    with
      interface Parser<'T, 'U> with
        member this.Apply(st0, kf, ks) =
          let kf = fun (st1, stack, msg) -> n.Apply(st0 + st1, kf, ks)
          m.Apply(State.noAdds st0, kf, ks)

  let (<|>) m n = OrP<_, _>(m, n) :> Parser<_, _>

  module private Lazy =

    let cons m (n: Lazy<_>) = m >>= (fun x -> n.Value |>> (fun xs -> x :: xs))

    type RightP<'T, 'U, 'V>(p: Parser<'T, 'U>, n: Lazy<Parser<'T, 'V>>) =
      override this.ToString() = IParser.infix (">>. " + n.ToString()) p
      with
        interface Parser<'T, 'V> with
          member this.Apply(st0, kf, ks) = p.Apply(st0, kf, fun (s, a) -> n.Value.Apply(s, kf, ks))

    let (>>.) p n = RightP<_,_, _>(p, n) :> Parser<_, _>

  let many p =
    let rec manyP = lazy (Lazy.cons p manyP <|> ok [])
    manyP.Value |> as_ ("many(" + p.ToString() + ")")

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
          put m.Mempty >>. wantInput
          >>= (fun more -> if more then inner (input :: acc) sp else ok (input :: acc))
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
    scan.Value |> as_ ("manyTill(" + p.ToString() + "," + q.ToString() + ")")
  
  let skipMany p =
    let rec scan = lazy (Lazy.(>>.) p scan <|> ok ())
    scan.Value |> as_ ("skipMany(" + p.ToString() + ")")

  let skipMany1 p = (p >>. skipMany p) |> as_ ("skipMany1(" + p.ToString() + ")")

  let sepBy1 p s =
    let rec scan = lazy (cons p (Lazy.(>>.) s scan <|> ok []))
    scan.Value |> as_ ("sepBy1(" + p.ToString() + "," + s.ToString() + ")")

  let sepBy p s =
    cons p ((s >>. sepBy1 p s) <|> ok []) <|> ok []
    |> as_ ("sepBy(" + p.ToString() + "," + s.ToString() + ")")

  let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> ok (f' m')
  let inline (<!>) f m = map f m
  let inline lift2 f a b = ok f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y

  let choice xs =
    List.foldBack (<|>) xs (error "choice") |> as_ ("choice(" + xs.ToString() + " :_*)")

  let opt p = attempt p |>> Some <|> ok None |> as_ ("opt(" + p.ToString() + ")")

  let between pBegin pEnd p = pBegin >>. p .>> pEnd

  type private MessageP<'T, 'U>(p: Parser<'T, 'U>, message: string) =
    override this.ToString() = IParser.infix ("<?> " + message) p
    with
      interface Parser<'T, 'U> with
        member this.Apply(st0, kf, ks) =
          let kf (st, strs, msg) = kf(st, msg :: strs, msg)
          p.Apply(st0, kf, ks)

  let (<?>) p message = MessageP<_, _>(p, message) :> Parser<_, _>

  let option x p = p <|> ok x

  type private RefP<'T, 'U>(refParser: Parser<'T, 'U> ref) =
    override this.ToString() = (!refParser).ToString()
    with
      interface Parser<'T, 'U> with
        member this.Apply(st, kf, ks) = (!refParser).Apply(st, kf ,ks)

  let createParserForwardedToRef () =
    let refParser = ref (error "Forwarded ref parser was never initialized")
    let fwdParser = RefP(refParser) :> Parser<_, _>
    (fwdParser, refParser)
