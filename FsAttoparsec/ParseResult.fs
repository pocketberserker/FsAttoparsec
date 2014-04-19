namespace Attoparsec

[<AbstractClass>]
type Monoid<'T>() =
  abstract member Mempty : 'T
  abstract member Mappend : 'T * 'T -> 'T

type ParseResult<'T, 'U> =
  | Fail of input: 'T * stack: string list * message: string
  | Partial of ('T -> ParseResult<'T, 'U>)
  | Done of input: 'T * result: 'U
  with
    member this.Map(f) =
      match this with
      | Fail(input, stack, message) -> Fail(input, stack, message)
      | Partial k -> Partial(fun s -> (k s).Map(f))
      | Done(input, result) -> Done(input, f result)

    member this.Feed(m: Monoid<_>, s) =
      match this with
      | Fail _ -> this
      | Partial k -> k s
      | Done(input, result) -> Done(m.Mappend(input, s), result)

    member this.Done_(m: Monoid<_>) =
      match this with
      | Fail _
      | Done _ -> this
      | Partial _ -> this.Feed(m, m.Mempty)

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

  let inline map f (result: ParseResult<_, _>) = result.Map(f)
  let inline feed m s (result: ParseResult<_, _>) = result.Feed(m, s)
  let inline done_ m (result: ParseResult<_, _>) = result.Done_(m)
  let inline option (result: ParseResult<_, _>) = result.Option
  let inline choice (result: ParseResult<_, _>) = result.Choice
