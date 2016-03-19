package kits

package free

package example

import org.scalatest.FunSuite

class FreeExample extends FunSuite {

  test("Reader") {
    def add1[U <: Union](implicit reader: Member[Reader[Int], U]): Free[U, Int] =
      for (a <- Reader.ask) yield a + 1
    assert(Free.run(Reader.run(add1[Reader[Int] :+: Void], 10)) == 11)
    def add11[U <: Union](implicit reader: Member[Reader[Int], U]): Free[U, Int] =
      Reader.local(add1)(((_: Int) + 10))
    assert(Free.run(Reader.run(add11[Reader[Int] :+: Void], 10)) == 21)
  }

  test("Writer") {
    def rdwr[U <: Union](implicit reader: Member[Reader[Int], U], writer: Member[Writer[Vector[String]], U]): Free[U, Int] =
      for {
        _ <- Writer.tell(Vector("begin"))
        n <- Reader.ask
        _ <- Writer.tell(Vector("end"))
      } yield n
    assert(Free.run(Writer.run(Reader.run(rdwr[Reader[Int] :+: Writer[Vector[String]] :+: Void], 10))) == (Vector("begin", "end"), 10))
    assert(Free.run(Reader.run(Writer.run(rdwr[Writer[Vector[String]] :+: Reader[Int] :+: Void]), 10)) == (Vector("begin", "end"), 10))
  }

  test("Error") {
    def tooBig[U <: Union](n: Int)(implicit error: Member[Error[Int], U]): Free[U, Int] =
      if (n <= 5)
        Error.fail(n)
      else
        Pure(n)
    assert(Free.run(Error.run(tooBig[Error[Int] :+: Void](3))) == Left(3))
    assert(Free.run(Error.run(tooBig[Error[Int] :+: Void](7))) == Right(7))
  }

  test("State") {
    def add10And20[U <: Union](implicit state: Member[State[Int], U]): Free[U, Int] =
      for {
        _ <- State.put(10)
        x <- State.get
        _ <- State.put(20)
        y <- State.get
      } yield x + y
    assert(Free.run(State.run(add10And20[State[Int] :+: Void], 0)) == (20, 30))
  }

  test("Choice") {
    import MonadPlus.Ops
    import Choice.FreeMonadPlus
    def even[U <: Union](implicit choice: Member[Choice[Vector], U]): Free[U, Int] = {
      type F[A] = Free[U, A]
      for {
        n <- Traverse.foldMap(1 to 10)(n => Pure(n): F[Int])
        if n % 2 == 0
      } yield n
    }
    assert(Free.run(Choice.run(even[Choice[Vector] :+: Void])) == Vector(2, 4, 6, 8, 10))
  }

  test("Stack safe") {
    def count[U <: Union](n: Int)(implicit state: Member[State[Int], U]): Free[U, Int] =
      if (n > 0)
        State.modify((_: Int) + 1).flatMap(_ => count(n - 1))
      else
        State.get
    assert(Free.run(State.eval(count[State[Int] :+: Void](10000), 0)) == 10000)
    def rdwr[U <: Union](n: Int)(implicit reader: Member[Reader[String], U], writer: Member[Writer[Vector[String]], U]): Free[U, Unit] =
      if (n > 0)
        for {
          s <- Reader.ask
          _ <- Writer.tell(Vector(s))
          _ <- rdwr(n - 1)
        } yield ()
      else
        Pure(())
    assert(Free.run(Reader.run(Writer.run(rdwr[Writer[Vector[String]] :+: Reader[String] :+: Void](10000)), "hoge"))._1.size == 10000)
  }

}
