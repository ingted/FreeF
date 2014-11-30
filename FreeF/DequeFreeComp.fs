namespace FreeF

type Deque = interface end
type Deque<'R, 'A, 'B> =
  inherit _3<Deque, 'R, 'A, 'B>

type FC<'F, 'A, 'B> = 'A -> Free<'F, 'B>
// FIXME
type FMExp<'F, 'A, 'B> = Deque<'F, 'A, 'B>

type FM<'S, 'X, 'A>(head: FreeView<'S, 'X>, tail: FMExp<'S, 'X, 'A>) =
  inherit Free<'S, 'A>()
  member val Head = head
  member val Tail = tail

// FIXME
type FCS<'S, 'A, 'B> = {
  F: 'A -> FreeF.Free<'S, 'B>
}
  with
    interface _2<'S, 'A, 'B>

open FSharp.Karma

[<AbstractClass>]
type DequeFreeComp(ts: TSequence<Deque>) =

  let (|FM|) (free: FreeF.Free<'S, 'A>) = free :?> FM<'S, 'A, 'A>

  member this.Viewer = { new FreeViewer<Free> with

    member x.FromView<'S, 'A>(h: FreeView<'S, 'A>) = FM(h, ts.Tempty<'S, 'A>() :?> Deque<_, _, _>) :> _2<_, _, _>

    member x.ToView<'S, 'A>(F: Functor<'S>, free: _2<Free, 'S, 'A>) =
      let apply x (a: _2<'S, 'x, 'y>) =
        match box a with
        | :? FCS<'S, 'x, 'y> as a -> a.F x
        | _ -> a :?> FreeF.Free<'S, 'y>
      let f = free :?> FM<'S, _, 'A>
      match f.Head with
      | Pure a ->
        match ts.Tviewl<'S, _, 'A>(f.Tail) with
        | :? TViewl.EmptyL<Deque, 'S, 'A> -> Pure a
        | _ as l ->
          let l = l :?> TViewl.LeafL<Deque, 'S, _, _, 'A>
          let f2 = apply x l.Head :?> FM<'S, _, _>
          x.ToView(F, FM(f2.Head, ts.Tappend<'S, _, _, 'A>(f2.Tail, l.Tail()) :?> Deque<_, _, _>))
      | Impure a ->
        let inner (FM f2) = FM(f2.Head, ts.Tappend<'S, _, _, 'A>(f2.Tail, f.Tail) :?> Deque<_, _, _>) :> FreeF.Free<_, _>
        Impure (F.Map(inner, a))
    }

  member this.bind<'S, 'A, 'B> (f: 'A -> FreeF.Free<'S, 'B>) (fa: FreeF.Free<'S, 'A>) =
    let free = fa :?> FM<'S, 'A, 'A>
    let deq = ts.Tappend<'S, _, 'A, 'B>(free.Tail, ts.Tsingleton<'S, 'A, 'B>({ F = f } :> _2<_, _, _>)) :?> Deque<_, _, 'B>
    FM(free.Head, deq) :> FreeF.Free<'S, 'B>

  member this.monad<'S> () =
    { new Monad<Free>() with
      member x.Point(a: F0<'A>) = this.Viewer.FromView(Pure (a.Apply())) :?> Free<'S, 'A> :> _1<Free, 'A>
      member x.Bind(StdF1 f, fa) =
        let inner a = f a :?> FreeF.Free<_, _>
        this.bind inner (fa :?> FreeF.Free<_, _>) :> _1<Free, _> }

  member this.liftF<'S, 'A> (s: Functor<'S>) (value: _1<'S, 'A>) =
    FreeF.Free.liftF value s this.Viewer

  member this.Trampoline = Trampoline(this.Viewer)
