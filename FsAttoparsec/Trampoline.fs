namespace Attoparsec

type Trampoline<'T> =
  | DelayValue of DelayT<'T>
  | ReturnValue of ReturnT<'T>
  | BindValue of IBind<'T>

and ITrampoline<'T> = 
  abstract member Value : Trampoline<'T>
  
and DelayT<'T>(f : unit -> ITrampoline<'T>) =
   member this.Func = f
   interface ITrampoline<'T> with
     member this.Value = DelayValue this
 
and ReturnT<'T>(x :'T) =
  member this.Value = x
  interface ITrampoline<'T> with
    member this.Value = ReturnValue this
 
and IBind<'T> = 
  abstract Bind<'U> : ('T -> ITrampoline<'U>) -> ITrampoline<'U>
and BindT<'T, 'U>(t: Trampoline<'T>, f : ('T -> ITrampoline<'U>)) =
  member this.Func = f
  member this.Value = t
  interface IBind<'U> with
    member this.Bind<'V>(f' : 'U -> ITrampoline<'V>) : ITrampoline<'V> =
      new BindT<_, _>(t, fun t -> new BindT<_, _>((f t).Value, (fun r -> f' r)) :> _) :> _
  interface ITrampoline<'U> with
    member this.Value = BindValue this

module Trampoline =

  let rec resume (t: ITrampoline<'T>) =
    match t with
    | :? DelayT<'T> as d -> Choice1Of2 d.Func
    | :? ReturnT<'T> as r -> Choice2Of2 r.Value
    | :? BindT<_, _> as b ->
      match t.Value with
      | ReturnValue a -> resume (b.Func a.Value)
      | DelayValue k -> Choice1Of2 (fun () -> new BindT<_, _>((k.Func ()).Value, b.Func) :> _)
      | BindValue b2 -> resume <| b2.Bind(b.Func)

  let rec run t =
    match resume t with
    | Choice1Of2 k -> run <| k ()
    | Choice2Of2 x -> x
