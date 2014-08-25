namespace Attoparsec

// port from https://github.com/pocketberserker/free-monad-fsharp/
// port from https://github.com/xuwei-k/free-monad-java/

type _1<'F, 'A> = interface end

type Functor<'F> =
  abstract member Map: ('A -> 'B) * _1<'F, 'A> -> _1<'F, 'B>

type Free<'F, 'A> =
  abstract member Bind: ('A -> Free<'F, 'B>) -> Free<'F, 'B>

[<Sealed>]
type private Gosub<'F, 'A, 'B> (a: Free<'F, 'A>, f: 'A -> Free<'F, 'B>) =
  member this.Value = a
  member this.Func = f
  interface Free<'F, 'B> with
    member this.Bind(g) = Gosub(a, fun a -> Gosub(f a, g) :> Free<'F, 'C>) :> Free<_, _>

[<Sealed>]
type private Done<'F, 'A> (a: 'A) =
  member this.Value = a
  interface Free<'F, 'A> with
    member this.Bind(f) = Gosub(this, f) :> Free<_, _>

[<Sealed>]
type private Suspend<'F, 'A> (a: _1<'F, Free<'F, 'A>>) =
  member this.Value = a
  interface Free<'F, 'A> with
    member this.Bind(f) = Gosub(this, f) :> Free<_, _>

module Free =

  let done_ b = Done(b) :> Free<_, _>
  let suspend a = Suspend(a) :> Free<_, _>
  let gosub f a = Gosub(a, f) :> Free<_, _>

  let rec resume<'X1, 'X2, 'F, 'A> (f: Functor<'F>) (free: Free<'F, 'A>) =
    match free with
    | :? Done<'F, 'A> as d -> Choice2Of2 d.Value
    | :? Suspend<'F, 'A> as s -> Choice1Of2 s.Value
    | _ ->
      let gosub1 = free :?> Gosub<'F, 'X1, 'A>
      match gosub1.Value with
      | :? Done<'F, 'X1> as d -> d.Value |> gosub1.Func |> resume f
      | :? Suspend<'F, 'X1> as d ->
        let g = fun (o: Free<_, _>) -> o.Bind(gosub1.Func)
        let value = f.Map(g, d.Value)
        Choice1Of2 value
      | _ ->
        let gosub2 = gosub1.Value :?> Gosub<'F, 'X2, 'X1>
        gosub2.Value.Bind(fun o -> (gosub2.Func o).Bind(gosub1.Func))
        |> resume f
        
  let liftF (g: Functor<'G>) (value: _1<'G, 'B>) = g.Map(done_, value) |> suspend

  let inline bind f (free: Free<_, _>) = free.Bind(f)
  let (>>=) m f = bind f m

  // port from https://github.com/scalaz/scalaz
  let rec run<'X1, 'X2, 'F, 'T> functor_ cast_ (free: Free<'F, 'T>) =
    match resume<'X1, 'X2, 'F, 'T> functor_ free with
    | Choice1Of2 k -> run functor_ cast_ (cast_ k)
    | Choice2Of2 a -> a

type F0 = F0

type F0<'T> = {
  Apply : unit -> 'T
}
  with
    interface _1<F0, 'T>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module F0 =

  let ofFunc f = { Apply = f }

  let toFunc f = f.Apply

  let map f f0 = { Apply = fun () -> f (f0.Apply ()) }

  let functor_ = { new Functor<F0> with
    member this.Map(f, fa: _1<F0, _>) = (fa :?> F0<_>) |> map f :> _1<_, _> }

  let run (x: _1<F0, Free<F0, 'T>>) = (x :?> F0<Free<F0, _>>).Apply ()

type Trampoline<'T> = Free<F0, 'T>

module Trampoline =

  let suspend (f: unit -> Trampoline<'T>) : Trampoline<_> = f |> F0.ofFunc |> Free.suspend

  let delay f : Trampoline<_> = suspend (fun () -> Free.done_ (f ()))
