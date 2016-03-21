package kits

package free

package example

import org.scalatest.FunSuite

class FreeExample extends FunSuite {

  test("Reader") {
    def e1[U <: Union: Reader[Int]#Member]: Free[U, Int] = for (a <- Reader.ask) yield a + 1
    val r1 = e1[Reader[Int] :+: Void] |> Reader.run(10) |> Free.run
    assert(r1 == 11)
    def e2[U <: Union: Reader[Int]#Member]: Free[U, Int] = Reader.local(e1)((_: Int) + 10)
    val r2 = e2[Reader[Int] :+: Void] |> Reader.run(10) |> Free.run
    assert(r2 == 21)
  }

  test("Writer") {
    def e1[U <: Union: Reader[Int]#Member: Writer[String]#Member]: Free[U, Int] =
      for {
        _ <- Writer.tell("begin")
        n <- Reader.ask
        _ <- Writer.tell("end")
      } yield n
    val r1 = e1[Reader[Int] :+: Writer[String] :+: Void] |> Reader.run(10) |> Writer.run |> Free.run
    assert(r1 == (Vector("begin", "end"), 10))
    val r2 = e1[Writer[String] :+: Reader[Int] :+: Void] |> Writer.run |> Reader.run(10) |> Free.run
    assert(r2 == (Vector("begin", "end"), 10))
  }

  test("Error") {
    def e1[U <: Union: Error[Int]#Member](n: Int): Free[U, Int] =
      if (n > 5)
        Error.fail(n)
      else
        Pure(n)
    val r1 = e1[Error[Int] :+: Void](3) |> Error.run |> Free.run
    assert(r1 == Right(3))
    val r2 = e1[Error[Int] :+: Void](7) |> Error.run |> Free.run
    assert(r2 == Left(7))
  }

  test("State") {
    def e1[U <: Union: State[Int]#Member]: Free[U, Int] =
      for {
        x <- State.get
        _ <- State.put(20)
        y <- State.get
      } yield x + y
    val r1 = e1[State[Int] :+: Void] |> State.run(10) |> Free.run
    assert(r1 == (20, 30))
  }

  test("Choice") {
    def e1[U <: Union: Choice#Member]: Free[U, Int] = {
      type F[A] = Free[U, A]
      for {
        n <- Traverse.foldMap(1 to 10)(n => Pure(n): F[Int])
        if n % 2 == 0
      } yield n
    }
    val r1 = e1[Choice :+: Void] |> Choice.run |> Free.run
    assert(r1 == Vector(2, 4, 6, 8, 10))
  }

}
