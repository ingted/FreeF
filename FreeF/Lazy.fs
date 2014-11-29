module FreeF.Lazy

[<AbstractClass>]
type ZList<'R, 'A, 'B>() =
  member this.Cons<'C>(pr: _2<'R, 'C, 'A>) = ZCons<'R, 'C, 'A, 'B>(pr, this)
  abstract member Append<'C> : ZList<'R, 'B, 'C> -> ZList<'R, 'A, 'C>
  abstract member IsEmpty : bool

and [<Sealed>] ZNil<'R, 'A>() =
  inherit ZList<'R, 'A, 'A>()
  override this.Append(other) = other
  override this.IsEmpty = true

and [<Sealed>] ZCons<'R, 'A, 'B, 'C>(head: _2<'R, 'A, 'B>, tail: ZList<'R, 'B, 'C>) =
  inherit ZList<'R, 'A, 'C>()
  override this.Append(other) = ZCons(head, tail.Append(other)) :> ZList<_, _, _>
  member val Head = head
  member val Tail = tail
  override this.IsEmpty = false

[<AbstractClass>]
type Digit<'R, 'A, 'B>() =
  abstract member ToList : unit -> ZList<'R, 'A, 'B>

[<Sealed>]
type One<'R, 'A, 'B>(a1: _2<'R, 'A, 'B>) =
  inherit Digit<'R, 'A, 'B>()
  override this.ToList() = ZCons(a1, ZNil<'R, 'B>()) :> ZList<_, _, _>
  member val A1 = a1

[<Sealed>]
type Two<'R, 'A, 'B, 'C>(a1: _2<'R, 'A, 'B>, a2: _2<'R, 'B, 'C>) =
  inherit Digit<'R, 'A, 'C>()
  override this.ToList() = ZCons(a1, ZCons(a2, ZNil<'R, _>())) :> ZList<_, _, _>
  member val A1 = a1
  member val A2 = a2

[<Sealed>]
type Three<'R, 'A, 'B, 'C, 'D>(a1: _2<'R, 'A, 'B>, a2: _2<'R, 'B, 'C>, a3: _2<'R, 'C, 'D>) =
  inherit Digit<'R, 'A, 'D>()
  override this.ToList() = ZCons(a1, ZCons(a2, ZCons(a3, ZNil<'R, _>()))) :> ZList<_, _, _>
  member val A1 = a1
  member val A2 = a2
  member val A3 = a3

[<Sealed>]
type Four<'R, 'A, 'B, 'C, 'D, 'E>(a1: _2<'R, 'A, 'B>, a2: _2<'R, 'B, 'C>, a3: _2<'R, 'C, 'D>, a4: _2<'R, 'D, 'E>) =
  inherit Digit<'R, 'A, 'E>()
  override this.ToList() = ZCons(a1, ZCons(a2, ZCons(a3, ZCons(a4, ZNil<'R, _>()))))  :> ZList<_, _, _>
  member val A1 = a1
  member val A2 = a2
  member val A3 = a3
  member val A4 = a4

[<AbstractClass>]
type Node<'R, 'A, 'B>() =
  abstract member ToDigit : unit -> Digit<'R, 'A, 'B>
  interface _2<'R, 'A, 'B>

type Node2<'R, 'A, 'B, 'C>(a1: _2<'R, 'A, 'B>, a2: _2<'R, 'B, 'C>) =
  inherit Node<'R, 'A, 'C>()
  override this.ToDigit() = Two(a1, a2) :> Digit<_, _, _>

type Node3<'R, 'A, 'B, 'C, 'D>(a1: _2<'R, 'A, 'B>, a2: _2<'R, 'B, 'C>, a3: _2<'R, 'C, 'D>) =
  inherit Node<'R, 'A, 'D>()
  override this.ToDigit() = Three(a1, a2, a3) :> Digit<_, _, _>

[<AbstractClass>]
type TFingerTree<'R, 'A, 'B>() =
  interface Deque<'R, 'A, 'B>

[<Sealed>]
type Empty<'R, 'A>() = inherit TFingerTree<'R, 'A, 'A>()

[<Sealed>]
type Single<'R, 'A, 'B>(a: unit -> _2<'R, 'A, 'B>) =
  inherit TFingerTree<'R, 'A, 'B>()
  member val A = a

[<Sealed>]
type Deep<'R, 'A, 'B, 'C, 'D>(prefix: unit -> Digit<'R, 'A, 'B>
  , middle: unit -> TFingerTree<'R, 'B, 'C>, suffix: Digit<'R, 'C, 'D>) =
  inherit TFingerTree<'R, 'A, 'D>()
  member val Prefix = prefix
  member val Middle = middle
  member val Suffix = suffix

let (|ZNil|_|) (o: ZList<'R, 'A, 'A>) =
  match o with
  | :? ZNil<'R, 'A> -> Some ()
  | _ -> None

let (|ZCons|_|) (o: ZList<'R, 'A, 'C>) =
  match o with
  | :? ZCons<'R, 'A, 'B, 'C> as c -> Some (c.Head, c.Tail)
  | _ -> None


let (|One|_|) (d: Digit<_, _, _>) =
  match d with
  | :? One<_, _, _> as o -> Some (o.A1)
  | _ -> None

let (|Two|_|) (d: Digit<_, _, _>) =
  match d with
  | :? Two<_, _, _, _> as t -> Some (t.A1, t.A2)
  | _ -> None

let (|Three|_|) (d: Digit<_, _, _>) =
  match d with
  | :? Three<_, _, _, _, _> as t -> Some (t.A1, t.A2, t.A3)
  | _ -> None
 
let (|Four|_|) (d: Digit<_, _, _>) =
  match d with
  | :? Four<_, _, _, _, _, _> as f -> Some (f.A1, f.A2, f.A3, f.A4)
  | _ -> None
  
module Digit =

  let append<'R, 'A, 'B, 'C> (d1: Digit<'R, 'A, 'B>) (d2: Digit<'R, 'B, 'C>) =
    match (d1, d2) with
    | (One d1 , One d2) -> Two(d1, d2) :> Digit<_, _, _>
    | (One d1 , Two(d2, d3)) -> Three(d1, d2, d3) :> Digit<_, _, _>
    | (Two(d1, d2) , One d3) -> Three(d1, d2, d3) :> Digit<_, _, _>
    | (One d1, Three(d2, d3, d4)) -> Four(d1, d2, d3, d4) :> Digit<_, _, _>
    | (Two(d1, d2) , Two(d3, d4)) -> Four(d1, d2, d3, d4) :> Digit<_, _, _>
    | (Three(d1, d2, d3), One(d4)) -> Four(d1, d2, d3, d4) :> Digit<_, _, _>
    | _ -> failwith "impossible case"

  let fromList<'R, 'A, 'B>(l: ZList<'R, 'A, 'B>) =
    match l with
    | ZCons(h1, t) ->
      match t with
      | ZNil -> One(h1) :> Digit<_, _, _>
      | ZCons(h2, t2) ->
        match t2 with
        | ZNil -> Two(h1, h2)  :> Digit<_, _, _>
        | ZCons(h3, t3) ->
          match t3 with
          | ZNil -> Three(h1, h2, h3)  :> Digit<_, _, _>
          | ZCons(h4, t5) ->
            match t5 with
            | ZNil -> Four(h1, h2, h3, h4)  :> Digit<_, _, _>
            | _ -> failwith "Unmanaged Too Long List"
          | _ -> failwith "impossible case"
        | _ -> failwith "impossible case"
      | _ -> failwith "impossible case"
    | _ -> failwith "impossible case"

  let inline toList (d: Digit<_, _, _>) = d.ToList()

module ZList =

  let inline append (z1: ZList<_, _, _>) (z2: ZList<_, _, _>) = z1.Append(z2)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TFingerTree =

  let empty<'R, 'A>() = Empty<'R, 'A>() :> TFingerTree<_, _, _>

  let single<'R, 'A, 'B>(a: _2<'R, 'A, 'B>) = Single<'R, 'A, 'B>(fun () ->a)  :> TFingerTree<_, _, _>

  let deep<'R, 'A, 'B, 'C, 'D> (prefix: Digit<'R, 'A, 'B>) (middle: TFingerTree<'R, 'B, 'C>) (suffix: Digit<'R, 'C, 'D>) =
    let p () = prefix
    let m () = middle
    Deep(p, m, suffix) :> TFingerTree<_, _, _>

  type Digit<'R, 'A, 'B> with
    // FIXME : type parameters
    member this.ToTree() =
      match this with
      | One a1 -> single a1
      | Two(a1, a2) -> deep (One(a1)) (empty ()) (One(a2))
      | Three(a1, a2, a3) -> deep (Two(a1, a2)) (empty ()) (One(a3))
      | Four(a1, a2, a3, a4) ->
        deep (Two(a1, a2)) (empty ()) (Two(a3, a4))
      | _ -> failwith "impossible case"

  // FIXME : type parameters
  let rec prepend<'R, 'A, 'B, 'C, 'u1, 'u2> (a: _2<'R, 'A, 'B>) (tree: TFingerTree<'R, 'B, 'C>) : TFingerTree<'R, 'A, 'C> =
    match box tree with
    | :? Empty<'R, 'A> -> single (downcast box a)
    | :? Single<'R, 'A, 'B> as t -> deep (One(a)) (empty ()) (One(downcast box (t.A())))
    | :? Deep<'R, 'B, 'B, 'C, 'C> as t ->
      match t.Prefix() with
      | :? Four<'R, 'B, 'A, 'u1, 'u2, 'B> as f ->
        let prefix = Two(a, f.A1)
        let middle = prepend (Node3(f.A2, f.A3, f.A4)) (t.Middle())
        deep prefix middle (downcast box t.Suffix)
      | _ ->
        let prefix = fun () -> Digit.append (One(a)) (t.Prefix())
        downcast box (Deep(prefix, t.Middle, t.Suffix))
    | _ -> failwith "impossible case"

  // FIXME : type parameters
  let rec append<'R, 'A , 'B, 'C, 'u1, 'u2> (tree: TFingerTree<'R, 'A, 'B>) (a: _2<'R, 'B, 'C>) : TFingerTree<'R, 'A, 'C> =
    match box tree with
    | :? Empty<'R, 'B> -> single (downcast box a)
    | :? Single<'R, 'A, 'B> as t -> deep (One(t.A())) (empty ()) (downcast box (One(a)))
    | :? Deep<'R, 'A, 'A, 'B, 'B> as t ->
      match t.Suffix with
      | :? Four<'R, 'B, 'u1, 'u2, 'C, 'B> as f ->
        let middle = t.Middle()
        let middle () = (append middle (Node3(f.A1, f.A2, f.A3)))
        Deep(t.Prefix, middle, Two(f.A4, a)) :> TFingerTree<_, _, _>
      | _ -> Deep(t.Prefix, t.Middle, Digit.append t.Suffix (One(a))) :> TFingerTree<_, _, _>
    | _ -> failwith "impossible case"

  // FIXME : type parameters
  let appendDeep<'R, 'A, 'B, 'C, 'D, 'E, 'u1, 'u2, 'u3> (tree: Deep<'R, 'A, 'B, 'C, 'D>) (a: _2<'R, 'D, 'E>) =
    match tree.Suffix with
    | :? Four<'R, 'C, 'u1, 'u2, 'u3, 'D> as f ->
      let middle = tree.Middle()
      let middle () = (append  middle (Node3(f.A1, f.A2, f.A3))) 
      Deep(tree.Prefix, middle, Two(f.A4, a)) :> TFingerTree<_, _, _>
    | _ -> Deep(tree.Prefix, tree.Middle, Digit.append tree.Suffix (One(a))) :> TFingerTree<_, _, _>

  // FIXME : type parameters
  let rec addAllL (l: ZList<'R, 'A, 'B>) (tree: TFingerTree<'R, 'B, 'C>) : TFingerTree<'R, 'A, 'C> =
    match l with
    | _ when l.IsEmpty -> downcast box tree
    | ZCons(h, t) -> prepend h (addAllL t tree)
    | _ -> failwith "impossible case"

  // FIXME : type parameters
  let rec addAllR (tree: TFingerTree<'R, 'A, 'B>) (l: ZList<'R, 'B, 'C>) : TFingerTree<'R, 'A, 'C> =
    match l with
    | _ when l.IsEmpty -> downcast box tree
    | ZCons(h, t) -> addAllR (append tree h) t
    | _ -> failwith "impossible case"

  // FIXME : type parameters
  let rec nodes (l: ZList<'R, 'A, 'B>) =
    match l with
    | ZCons(h1, t1) ->
      match t1 with
      | ZCons(h2, t2) ->
        match t2 with
        | ZNil -> ZCons(Node2(h1, h2), ZNil()) :> ZList<_, _, _>
        | ZCons(h3, t3) ->
          match t3 with
          | ZNil -> ZCons(Node3(h1, h2, h3), ZNil()) :> ZList<_, _, _>
          | ZCons(h4, t4) ->
            match t4 with
            | ZNil -> ZCons(Node2(h1, h2), ZCons(Node2(h3, h4), ZNil()))  :> ZList<_, _, _>
            | _ -> ZCons(Node3(h1, h2, h3), (downcast box (nodes (downcast box t3)))) :> ZList<_, _, _>
          | _ -> failwith "impossible case"
        | _ -> failwith "Unmanaged Case"
      | _ -> failwith "Unmanaged Case"
    | _ -> failwith "impossible case"

  // FIXME : type parameters
  let rec app3 (t1: TFingerTree<'R, 'A, 'B>) (l: ZList<'R, 'B, 'C>) (t2: TFingerTree<'R, 'C, 'D>) : TFingerTree<'R, 'A, 'D> =
    match box t1 with
    | :? Empty<'R, 'A> -> addAllL (downcast box l) t2
    | :? Single<'R, 'A, 'B> as t11 -> prepend (downcast box (t11.A())) (addAllL (downcast box l) t2)
    | :? Deep<'R, 'A, 'A, 'B, 'B> as t11 ->
      match box t2 with
      | :? Empty<'R, 'C> -> addAllR (downcast box t1) (downcast box l)
      | :? Single<'R, 'C, 'D> as t22 -> append (addAllR t1 (downcast box l)) (downcast box (t22.A()))
      | :? Deep<'R, 'C, 'C, 'D, 'D> as t22 ->
        let middle () =
          app3 (t11.Middle())
            (nodes (ZList.append (Digit.toList t11.Suffix) (ZList.append l (Digit.toList (t22.Prefix())))))
            (t22.Middle())
        Deep(t11.Prefix, middle, t22.Suffix) :> TFingerTree<_, _, _>
      | _ -> failwith "impossible case"
    | _ -> failwith "impossible case"

  // FIXME : type parameters
  let app2 (t1: TFingerTree<'R, 'A, 'B>) (t2: TFingerTree<'R, 'B, 'C>) : TFingerTree<'R, 'A, 'C> =
    match box t1 with
    | :? Empty<'R, 'A> -> downcast box t2
    | :? Single<'R, 'A, 'B> as t11 -> prepend (downcast box (t11.A())) (downcast box t2)
    | :? Deep<'R, 'A, 'A, 'B, 'B> as t11 ->
      match box t2 with
      | :? Empty<'R, 'B> -> downcast box t1
      | :? Single<'R, 'B, 'C> as t22 -> appendDeep (downcast box t11) (t22.A())
      | :? Deep<'R, 'B, 'B, 'C, 'C> as t22 ->
        let middle () =
          app3 (t11.Middle()) (nodes (ZList.append (Digit.toList t11.Suffix) (Digit.toList (t22.Prefix())))) (t22.Middle())
        downcast box (Deep(t11.Prefix, middle, t22.Suffix))
      | _ -> failwith "impossible case"
    | _ -> failwith "impossible case"

  // FIXME : type parameters
  let deepL<'R, 'A, 'B, 'C, 'D, 'u> (pr: ZList<'R, 'A, 'B>) (m: TFingerTree<'R, 'B, 'C>) (sf: Digit<'R, 'C, 'D>) (ts: TSequence<Deque>) : TFingerTree<'R, 'A, 'D> =
    match box pr with
    | :? ZNil<'R, 'A> ->
      match ts.Tviewl(m) with
      | :? TViewl.EmptyL<Deque, 'B, 'B> -> downcast box (sf.ToTree())
      | :? TViewl.LeafL<Deque, 'R, 'B, 'u, 'C> as l ->
        let prefix () = downcast box ((l.Head :?> Node<'R, 'B, 'u>).ToDigit())
        let middle () = (l.Tail() :?> TFingerTree<_, _, _>)
        Deep(prefix, middle, sf) :> TFingerTree<_, _, _>
      | _ -> failwith "impossible case"
    | _ ->
      let prefix () = Digit.fromList pr
      let middle () = m
      Deep(prefix, middle, sf) :> TFingerTree<_, _, _>

  // FIXME: compile error
  let tseq = { new TSequence<Deque> with
    member this.Tempty() = empty () :> _3<Deque, _, _, _>

    member this.Tsingleton(c) = single c :> _3<Deque, _, _, _>

    member this.Tappend(a, b) =
      app2 (a :?> TFingerTree<_, _, _>) (b :?> TFingerTree<_, _, _>)
      :> _3<Deque, _, _, _>

    member this.Tviewl(s: _3<Deque, 'C, 'X, 'Y>) =
      match box s with
      | :? Empty<'C, 'X> -> downcast box (TViewl.emptyL ())
      | :? Single<'C, 'X, 'Y> as t -> TViewl.leafL (t.A()) (fun () -> downcast box (empty ()))
      | :? Deep<'C, 'X, 'X, 'Y, 'Y> as t ->
        match Digit.toList (t.Prefix()) with
        | ZCons(hh, tt) ->
          TViewl.leafL hh (fun () -> deepL tt (t.Middle()) t.Suffix this :> _3<Deque, _, _, _>)
        | _ -> failwith "impossible case"
      | _ -> failwith "impossible case"
  }

[<Sealed>]
type  Free internal () = inherit DequeFreeComp(downcast box TFingerTree.tseq)

// module
let Free = Free ()
