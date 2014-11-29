namespace FSharp.FreeF.Tests

open NUnit.Framework
open FsUnit
open FreeF.Strict

[<TestFixture>]
module StrictFreeTest =

  type FreeBuilder () =
    member this.Return(x) = Free.Trampoline.done_ x
    member this.ReturnFrom(x) = x
    member this.Bind(x, f) = Free.bind f x

  let free = FreeBuilder()

  let rec fib n =
    if n < 2M then Free.Trampoline.done_ n
    else
      free {
        let! x = Free.Trampoline.suspend (fun () -> fib (n - 1M))
        let! y = Free.Trampoline.suspend (fun () -> fib (n - 2M))
        return x + y
      }

  module KF0 = FSharp.Karma.F0

  let run<'X1, 'X2, 'T> f = Free.Trampoline.run KF0.functor_ f

  [<Test>]
  let ``fib test`` () =
    fib 6M
    |> run<decimal, decimal, decimal>
    |> should equal 8M

  //[<Test>]
  //let ``more fib test`` () =
  //  fib 35M
  //  |> run<decimal, decimal, decimal>
  //  |> should equal 9227465M

  let rec factorial n =
    if n < 2M then Free.Trampoline.done_ 1M
    else
      free {
        let! x = Free.Trampoline.suspend (fun () -> factorial (n - 1M))
        return n * x
      }

  [<Test>]
  let ``factorial test`` () =
    factorial 5M
    |> run<decimal, decimal, decimal>
    |> should equal 120M
