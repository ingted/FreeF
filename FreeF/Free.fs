namespace FreeF

open FSharp.Karma

type FreeView<'S, 'A> =
  | Pure of 'A
  | Impure of _1<'S, Free<'S, 'A>>

and FreeViewer<'Free> =
  abstract member FromView<'S, 'A> : FreeView<'S, 'A> -> _2<'Free, 'S, 'A>
  abstract member ToView<'S, 'A> : Functor<'S> * _2<'Free, 'S, 'A> -> FreeView<'S, 'A>

and [<AbstractClass>] Free<'S, 'A>() =

  member this.Map(m: Monad<_>, f) = m.Map(f, this)
  member this.Bind(m: Monad<_>, f) = m.Bind(f, this)

  member this.MapSuspension<'T>(f: NT<'S, 'T>, s: Functor<'S>, t: Functor<'T>, v: FreeViewer<Free>) =
    v.FromView(
      match v.ToView(s, this :> _2<_, _, _>) with
      | Pure a -> Pure a
      | Impure a ->
        let inner (tf: Free<_, _>) = tf.MapSuspension(f, s, t, v) :?> Free<'T, 'A>
        Impure (f.Apply(s.Map(inner,  a)))
    )

  member this.FoldMap<'M>(f: NT<'S, 'M>, s: Functor<'S>, m: Monad<'M>, v: FreeViewer<Free>) =
    match v.ToView(s, this :> _2<_, _, _>) with
    | Pure a -> m.Point(F0.ofFunc <| fun () -> a)
    | Impure a ->
      let inner = F1.ofFunc <| fun (x: Free<_, _>) -> x.FoldMap(f, s, m, v)
      m.Bind(inner, f.Apply(a))

  member this.Go(f: _1<'S, Free<'S, 'A>> -> Free<'S, 'A>, s: Functor<'S>, v: FreeViewer<Free>) =
    let rec go2 (t: Free<'S, 'A>) =
      match v.ToView(s, t :> _2<_, _, _>) with
      | Impure a -> go2 (f a)
      | Pure a -> a
    go2 this

  interface _1<Free, 'A> 
  interface _2<Free, 'S, 'A>

type Trampoline<'A> = Free<F0, 'A>

module Free =

  let liftF<'S, 'A> (value: _1<'S, 'A>) (s: Functor<'S>) (v: FreeViewer<Free>) =
    let inner v2 = v.FromView<'S, 'A>(Pure v2) :?> Free<'S, 'A>
    v.FromView<'S, 'A>(Impure (s.Map(inner , value))) :?> Free<'S, 'A>

[<Sealed>]
type Trampoline(v: FreeViewer<Free>) =

  member this.done_ (a: 'A) =
    v.FromView<F0, 'A>(Pure a) :?> Trampoline<'A>

  member this.suspend (a: unit -> Trampoline<'A>) =
    v.FromView<F0, 'A>(Impure (F0.ofFunc <| a)) :?> Trampoline<'A>

  member this.delay (a: 'A) = this.suspend (fun () -> this.done_ a)

  member this.run s (tr: Trampoline<'A>) =
    let inner (f: _1<_, _>) = (f :?> F0<Free<F0, 'A>>).Apply()
    tr.Go(inner, s, v)
