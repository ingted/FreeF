namespace FreeF

type TSequence<'S> =
  abstract member Tempty<'C, 'X> : unit -> _3<'S, 'C, 'X, 'X>
  abstract member Tsingleton<'C, 'X, 'Y> : _2<'C, 'X, 'Y> -> _3<'S, 'C, 'X, 'Y>
  abstract member Tappend<'C, 'X, 'Y, 'Z> : _3<'S, 'C, 'X, 'Y> * _3<'S, 'C, 'Y, 'Z> -> _3<'S, 'C, 'X, 'Z>
  abstract member Tviewl<'C, 'X, 'Y> : _3<'S, 'C, 'X, 'Y> -> TViewl<'S, 'C, 'X, 'Y>
