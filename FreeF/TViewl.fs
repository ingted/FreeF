namespace FreeF

type TViewl<'S, 'C, 'X, 'Y> = interface end

module TViewl =

  type EmptyL<'S, 'C, 'X>() =
    interface TViewl<'S, 'C, 'X, 'X>

  type LeafL<'S, 'C, 'X, 'Y, 'Z>(head: _2<'C, 'X, 'Y>, tail: unit -> _3<'S, 'C, 'Y, 'Z>) =
    member val Head = head
    member val Tail = tail
    interface TViewl<'S, 'C, 'X, 'Z>

  let emptyL () = EmptyL() :> TViewl<_, _, _, _>

  let leafL h t = LeafL(h, t) :> TViewl<_, _, _, _>
