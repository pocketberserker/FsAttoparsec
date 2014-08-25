namespace Attoparsec

type Pos = int

type State<'T> = {
  Input: 'T
  Pos: Pos
  Complete: bool
  Monoid: Monoid<'T>
}

module State =
  let completed s = { s with Complete = true }

module Internal =

  type Result<'T, 'U> =
    | Fail of 'T * string list * string
    | Partial of ('T -> Trampoline<Result<'T, 'U>>)
    | Done of 'T * 'U
    with
      member this.Translate =
        match this with
        | Fail(input, stack, message) -> ParseResult.Fail(input, stack, message)
        | Partial k -> ParseResult.Partial (fun a -> (Free.run F0.functor_ F0.run (k a)).Translate)
        | Done(input, result) -> ParseResult.Done(input, result)

  module Result =
    let inline translate (result: Result<_, _>) = result.Translate

type Failure<'T, 'U> = State<'T> * string list * string -> Trampoline<Internal.Result<'T, 'U>>
type Success<'T, 'U, 'V> = State<'T> * 'U -> Trampoline<Internal.Result<'T, 'V>>
  
type Parser<'T, 'U> =
  abstract member Apply: State<'T> * Failure<'T, 'V> * Success<'T, 'U, 'V> -> Trampoline<Internal.Result<'T, 'V>>

module private IParser =
  let infix s p = "(" + p.ToString() + ") " + s

[<Sealed>]
type private BindP<'T, 'U, 'V>(p: Parser<'T, 'U>, f: 'U -> Parser<'T, 'V>) =
  override this.ToString() = IParser.infix "bind ..." p
  with
    interface Parser<'T, 'V> with
      member this.Apply(st0, kf, ks) =
        let ks = fun (s, a) -> (f a).Apply(s, kf, ks)
        Trampoline.suspend <| fun () -> p.Apply(st0, kf, ks)

[<Sealed>]
type private MapP<'T, 'U, 'V>(p: Parser<'T, 'U>, f: 'U -> 'V) =
  override x.ToString() =  IParser.infix "map ..." p
  with
    interface Parser<'T, 'V> with
      member x.Apply(st0, kf, ks) =
        Trampoline.suspend <| fun () -> p.Apply(st0, kf, (fun (s, a) -> ks (s, f a)))

[<Sealed>]
type private FilterP<'T, 'U>(p: Parser<'T, 'U>, pred: 'U -> bool) =
  override x.ToString() =  IParser.infix "filter ..." p
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) =
        p.Apply(st0, kf, (fun (s, a) -> if pred a then ks (s, a) else kf(s, [], "withFilter")))

[<Sealed>]
type private AsP<'T, 'U>(p: Parser<'T, 'U>, s: string) =
  override x.ToString() = s
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) =
        let kf = fun (st1, stack, msg) -> kf (st1, s :: stack, msg)
        p.Apply(st0, kf, ks)

[<Sealed>]
type private AsOpaqueP<'T, 'U>(p: Parser<'T, 'U>, s: string) =
  override x.ToString() = s
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, ks) =
        let kf = fun (st1, stack, msg) -> kf (st1, [], "Failure reading:" + s)
        p.Apply(st0, kf, ks)

[<Sealed>]
type private ReturnP<'T, 'U>(a: 'U) =
  override x.ToString() =
    match box a with
    | null -> "ok(unit or null)"
    | _ -> "ok(" + a.ToString() + ")"
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, _, ks) = ks (st0, a)

[<Sealed>]
type private ErrorP<'T, 'U>(what: string) =
  override x.ToString() = "error(" + what + ")"
  with
    interface Parser<'T, 'U> with
      member x.Apply(st0, kf, _) = kf (st0, [], "Failed reading: " + what)

[<AutoOpen>]
module Parser =

  open Internal

  let infix s p = IParser.infix s p

  let parse skip (m: Monoid<_>) (p: Parser<_, _>) input =
    let st = {
      Input = input
      Pos = 0
      Complete = false
      Monoid = m
    }
    let kf = fun (a, b, c) -> Free.done_ <| Internal.Fail(skip a.Pos a.Input, b, c)
    let ks = fun (a, b) -> Free.done_ <| Internal.Done(skip a.Pos a.Input, b)
    p.Apply(st, kf, ks)
    |> Free.run F0.functor_ F0.run
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

  let parseOnly skip (m: Monoid<_>) (parser: Parser<_, _>) input =
    let state = { Input = input; Pos = 0; Complete = true; Monoid = m }
    let kf = fun (a, b, c) -> Free.done_ <| Internal.Fail(skip a.Pos a.Input, b, c)
    let ks = fun (a, b) -> Free.done_ <| Internal.Done(skip a.Pos a.Input, b)
    match Free.run F0.functor_ F0.run <| parser.Apply(state, kf, ks) with
    | Fail(_, _, e) -> Choice2Of2 e
    | Done(_, a) -> Choice1Of2 a
    | Partial _ -> Choice2Of2 "parseOnly: Parser returned a partial result"

  type private AdvanceP<'T>(n: int) =
    override this.ToString() = "advance(" + string n + ")"
    with
      interface Parser<'T, unit> with
        member this.Apply(st0, _, ks) =
          ks ({ st0 with Pos = st0.Pos + n }, ())

  let advance n = AdvanceP(n) :> Parser<_, unit>

  let prompt st kf ks = Partial (fun s ->
    let m = st.Monoid
    if s = m.Mempty then Trampoline.suspend <| fun () -> kf { st with Complete = true }
    else Trampoline.suspend <| fun () -> ks { st with Input = m.Mappend(st.Input, s); Complete = false })

  [<Sealed>]
  type private DemandInputP<'T when 'T : equality>() =
    override this.ToString() = "demandInput"
    with
      interface Parser<'T, unit> with
        member this.Apply(st0, kf, ks) =
          if st0.Complete then
            Trampoline.suspend <| fun () -> kf (st0, [], "not enough input")
          else
            Free.done_ <| prompt st0 (fun st -> kf (st, [], "not enough input")) (fun a -> ks (a, ()))

  let inline private demandInput'<'T when 'T : equality> () = DemandInputP<'T>() :> Parser<'T, unit>
  let demandInput<'T when 'T : equality> = demandInput'<'T> ()

  [<Sealed>]
  type private RightP<'T, 'U, 'V>(p: Parser<'T, 'U>, n: Parser<'T, 'V>) =
    override this.ToString() = IParser.infix (">>. " + n.ToString()) p
    with
      interface Parser<'T, 'V> with
        member this.Apply(st0, kf, ks) = p.Apply(st0, kf, fun (s, a) -> n.Apply(s, kf, ks))

  let (>>.) (p: Parser<_, _>) (n: Parser<_, _>) = RightP<_, _, _>(p, n) :> Parser<_, _>

  [<Sealed>]
  type private LeftP<'T, 'U, 'V>(m: Parser<'T, 'U>, n: Parser<'T, 'V>) =
    override this.ToString() = IParser.infix (".>> " + n.ToString()) m
    with
      interface Parser<'T, 'U> with
        member this.Apply(st0, kf, ks) = m.Apply(st0, kf, fun (st1, a) -> n.Apply(st1, kf, fun (st2, b) -> ks (st2, a)))
  
  let (.>>) (m: Parser<_, _>) (n: Parser<_, _>) = LeftP<_, _, _>(m, n) :> Parser<_, _>

  [<Sealed>]
  type private EnsureSuspendedP<'T when 'T : equality>(length: 'T -> int, sub: int -> int -> 'T -> 'T, st: State<'T>, n: int) =
    override this.ToString() = "ensureSuspended(" + string n + ")"
    with
      interface Parser<'T, 'T> with
        member this.Apply(st0, kf, ks) =
          if length st0.Input >= st0.Pos + n then ks (st0, sub st.Pos n st0.Input)
          else (demandInput >>. EnsureSuspendedP(length, sub, st0, n)).Apply(st0, kf, ks)

  let ensureSuspended length sub st n = EnsureSuspendedP(length, sub, st, n) :> Parser<_, _>

  [<Sealed>]
  type private EnsureP<'T when 'T : equality>(length: 'T -> int, sub: int -> int -> 'T -> 'T, n: int) =
    override this.ToString() = "ensure(" + string n + ")"
    with
      interface Parser<'T, 'T> with
        member this.Apply(st0, kf, ks) =
          if length st0.Input >= st0.Pos + n then ks(st0, sub st0.Pos n st0.Input)
          else (ensureSuspended length sub st0 n).Apply(st0, kf, ks)

  let ensure (length: 'T -> int) sub n = EnsureP<_>(length, sub, n) :> Parser<_, _>

  [<Sealed>]
  type private WantInputP<'T when 'T : equality>(length: 'T -> int) =
    override this.ToString() = "wantInput"
    with
      interface Parser<'T, bool> with
        member this.Apply(st0, kf, ks) =
          if length st0.Input >= st0.Pos + 1 then ks (st0, true)
          elif st0.Complete then ks (st0, false)
          else Free.done_ <| prompt st0 (fun a -> ks (a, false)) (fun a -> ks (a, true))

  let wantInput length = WantInputP<_>(length) :> Parser<_, _>

  let atEnd length = wantInput length |>> not

  [<Sealed>]
  type private GetP<'T>(skip: int -> 'T -> 'T) =
    override this.ToString() = "get"
    with
      interface Parser<'T, 'T> with
        member this.Apply(st0, _, ks) = ks(st0, skip st0.Pos st0.Input)

  let get skip = GetP(skip) :> Parser<_, _>

  let inline attempt p = p

  let elem length head sub p what =
    let what = match what with | Some s -> s | None -> "elem(...)"
    ensure length sub 1
    >>= fun s ->
      let c = head s
      if p c then advance 1 >>. ok c
      else error what
    |> asOpaque what

  [<Sealed>]
  type private EndOfChunkP<'T>(length: 'T -> int) =
    override this.ToString() = "endOfChunk"
    with
      interface Parser<'T, bool> with
        member this.Apply(st0, _, ks) =
          ks (st0, st0.Pos = length st0.Input)

  let endOfChunk length = EndOfChunkP(length) :> Parser<_, _>

  let satisfy length head sub p = elem length head sub p (Some "satisfy(...)")

  let skip length head sub p what =
    let what = match what with | Some s -> s | None -> "skip(...)"
    ensure length sub 1
    >>= fun s ->
      if p (head s) then advance 1
      else error what
    |> asOpaque what

  let rec skipWhile (m: Monoid<_>) skipWhile' skip length (p: _ -> bool) : Parser<_, unit> = parser {
    let! t = get skip |>> (skipWhile' p)
    do! advance (length t)
    let! eoc = endOfChunk length
    let! input = if eoc then wantInput length else zero
    return!
      if input then skipWhile m skipWhile' skip length p
      else zero
  }

  let takeWith length sub n p what =
    let what = match what with | Some s -> s | None -> "takeWith(...)"
    let n = max n 0
    ensure length sub n
    >>= fun s ->
      if p s then advance n >>. ok s
      else error what
    |> asOpaque what

  let take length sub n =
    takeWith length sub n (fun _ -> true) (Some ("take(" + (string n) + ")"))

  let takeWhile (m: Monoid<_>) takeWhile length skip (p: _ -> bool) : Parser<_, _> =
    let rec inner acc = parser {
      let! x = get skip |>> (takeWhile p)
      do! advance (length x)
      let! eoc = endOfChunk length
      return!
        if eoc then
          wantInput length
          >>= function
            | true -> inner (m.Mappend(x, acc))
            | false -> ok (m.Mappend(x, acc))
        else ok (m.Mappend(x, acc)) }
    inner m.Mempty

  let takeRest (m: Monoid<_>) length skip =
    let rec inner acc = parser {
      let! input = wantInput length
      return!
        if input then get skip >>= fun s -> advance (length s) >>. inner (s :: acc)
        else ok (List.rev acc)
    }
    inner []

  let takeText (m: Monoid<_>) length skip fold =
    takeRest m length skip |>> fold (fun a b -> m.Mappend(a, b)) m.Mempty

  let private when' (m: Parser<_, unit>) b = if b then m else ok ()

  let takeWhile1 (m: Monoid<_>) takeWhile' length skip p = parser {
    do! endOfChunk length >>= when' demandInput
    let! s = get skip |>> (takeWhile' p)
    let len = length s
    return!
      if len = 0 then error "takeWhile1"
      else
        advance len
        >>. endOfChunk length
        >>= fun eoc ->
          if eoc then takeWhile m takeWhile' length skip p |>> (fun x -> m.Mappend(s, x))
          else ok s
  }

  [<Sealed>]
  type private EndOfInputP<'T when 'T : equality>(length: 'T -> int) =
    override this.ToString() = "endOfInput"
    with
      interface Parser<'T, unit> with
        member this.Apply(st0, kf, ks) =
          if st0.Pos < length st0.Input then kf (st0, [], "endOfInput")
          elif st0.Complete then ks (st0, ())
          else
            let kf = fun (st1, _, _) -> ks (st1, ())
            let ks = fun (st1, _) -> kf (st1, [], "endOfInput")
            demandInput.Apply(st0, kf, ks)

  let endOfInput length = EndOfInputP<_>(length) :> Parser<_, unit>
  
  let phrase length p = p .>> endOfInput length |> as_ ("phrase" + p.ToString())
 
  [<Sealed>]
  type private ConsP<'T, 'U, 'V, 'W>(m: Parser<'T, 'U>, n: unit -> Parser<'T, 'V>, cons: 'U -> 'V -> 'W) =
    override x.ToString() =  "(" + m.ToString() + ") :: (" + (n ()).ToString() + ")"
    with
      interface Parser<'T, 'W> with
        member x.Apply(st0, kf, ks) =
          m.Apply(st0, kf, fun (s, a) -> (n ()).Apply(s, kf, (fun (s, b) -> ks (s, cons a b))))

  let cons inputCons m n = ConsP(m, (fun () -> n), inputCons) :> Parser<_, _>

  [<Sealed>]
  type private OrP<'T, 'U>(m: Parser<'T, 'U>, n: Parser<'T, 'U>) =
    override this.ToString() = IParser.infix ("<|> ...") m
    with
      interface Parser<'T, 'U> with
        member this.Apply(st0, kf, ks) =
          let kf = fun (st1, _, _) -> n.Apply({ st1 with Pos = st0.Pos }, kf, ks)
          m.Apply(st0, kf, ks)

  let (<|>) m n = OrP<_, _>(m, n) :> Parser<_, _>

  module private Lazy =

    let cons inputCons m (n: Lazy<_>) = ConsP(m, (fun () -> n.Value), inputCons) :> Parser<_, _>

    [<Sealed>]
    type RightP<'T, 'U, 'V>(p: Parser<'T, 'U>, n: Lazy<Parser<'T, 'V>>) =
      override this.ToString() = IParser.infix (">>. " + n.ToString()) p
      with
        interface Parser<'T, 'V> with
          member this.Apply(st0, kf, ks) = p.Apply(st0, kf, fun (s, a) -> n.Value.Apply(s, kf, ks))

    let (>>.) p n = RightP<_,_, _>(p, n) :> Parser<_, _>

  let many (monoid: Monoid<_>) inputCons p =
    let rec manyP = lazy (Lazy.cons inputCons p manyP <|> ok monoid.Mempty)
    manyP.Value |> as_ ("many(" + p.ToString() + ")")

  let many1 monoid inputCons p = cons inputCons p (many monoid inputCons p)

  type private Scan<'T, 'U> =
    | Continue of 'T
    | Finished of int * 'U

  let scan (m: Monoid<_>) head tail take length skip s (p: _ -> _ -> Option<_>) =
    let rec scanner s n t =
      if t = m.Mempty then Continue s
      else
        match p s (head t) with
        | Some s -> scanner s (n + 1) (tail t)
        | None -> Finished(n, t)
    let rec inner acc s = parser {
      let! input = get skip
      return!
        match scanner s 0 input with
        | Continue sp ->
          advance (length input)
          >>. endOfChunk length
          >>= function
            | true ->
              wantInput length >>= fun more ->
                if more then inner (input :: acc) sp else ok (input :: acc)
            | false -> zero
        | Finished(n, t) ->
          let i = input |> take n
          advance (length input - length t) >>. ok (i :: acc)
    }
    parser {
      let! chunks = inner [] s
      return!
        match chunks with
        | [x] -> ok x
        | xs -> ok (xs |> List.rev |> List.fold (fun a b -> m.Mappend(a, b)) m.Mempty)
    }

  let manyTill (monoid: Monoid<_>) inputCons p q =
    let rec scan = lazy (q >>. ok monoid.Mempty <|> Lazy.cons inputCons p scan)
    scan.Value |> as_ ("manyTill(" + p.ToString() + "," + q.ToString() + ")")
  
  let skipMany p =
    let rec scan = lazy (Lazy.(>>.) p scan <|> ok ())
    scan.Value |> as_ ("skipMany(" + p.ToString() + ")")

  let skipMany1 p = (p >>. skipMany p) |> as_ ("skipMany1(" + p.ToString() + ")")

  let sepBy1 (monoid: Monoid<_>) inputCons p s =
    let rec scan = lazy (cons inputCons p (Lazy.(>>.) s scan <|> ok monoid.Mempty))
    scan.Value |> as_ ("sepBy1(" + p.ToString() + "," + s.ToString() + ")")

  let sepBy (monoid: Monoid<_>) inputCons p s =
    cons inputCons p ((s >>. sepBy1 monoid inputCons p s) <|> ok monoid.Mempty) <|> ok monoid.Mempty
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

  [<Sealed>]
  type private MessageP<'T, 'U>(p: Parser<'T, 'U>, message: string) =
    override this.ToString() = IParser.infix ("<?> " + message) p
    with
      interface Parser<'T, 'U> with
        member this.Apply(st0, kf, ks) =
          let kf (st, strs, msg) = kf(st, msg :: strs, msg)
          p.Apply(st0, kf, ks)

  let (<?>) p message = MessageP<_, _>(p, message) :> Parser<_, _>

  let option x p = p <|> ok x

  [<Sealed>]
  type private RefP<'T, 'U>(refParser: Parser<'T, 'U> ref) =
    override this.ToString() = (!refParser).ToString()
    with
      interface Parser<'T, 'U> with
        member this.Apply(st, kf, ks) = (!refParser).Apply(st, kf ,ks)

  let createParserForwardedToRef () =
    let refParser = ref (error "Forwarded ref parser was never initialized")
    let fwdParser = RefP(refParser) :> Parser<_, _>
    (fwdParser, refParser)

  [<Sealed>]
  type private Tuple2P<'T, 'U, 'V>(p1: Parser<'T, 'U>, p2: Parser<'T, 'V>) =
    override this.ToString() = IParser.infix (".>>. " + p2.ToString()) p1
    with
      interface Parser<'T, 'U * 'V> with
        member this.Apply(st0, kf, ks) =
          let ks (s, a) = p2.Apply(s, kf, (fun (s, b) -> Trampoline.suspend <| fun () -> ks (s, (a, b))))
          Trampoline.suspend <| fun () -> p1.Apply(st0, kf, ks)

  let tuple2 p1 p2 = Tuple2P(p1, p2)  :> Parser<_, _>
  let inline (.>>.) p1 p2 = tuple2 p1 p2

  [<Sealed>]
  type private MatchP<'T, 'U>(p: Parser<'T, 'U>, sub: int -> int -> 'T -> 'T) =
    override this.ToString() = "match(" + p.ToString() + ")"
    interface Parser<'T, 'T * 'U> with
      member this.Apply(st0, kf, ks) =
        let ks = fun (s, a) -> ks (s, (sub st0.Pos (s.Pos - st0.Pos) s.Input, a))
        p.Apply(st0, kf, ks)

  let match_ sub p = MatchP(p, sub) :> Parser<_, _>

  [<Sealed>]
  type private PositionP<'T>() =
    override this.ToString() = "getPosition"
    with
      interface Parser<'T, int> with
        member this.Apply(st0, _, ks) = ks (st0, st0.Pos)

  let getPosition<'T> = PositionP<'T>() :> Parser<_, _>
