package kits

package free

package example

import org.scalatest.FunSuite

class FreeExample extends FunSuite {

  test("Reader") {
    def e1[U <: Union: Reader[Int]#Member]: Free[U, Int] = for (a <- Reader.ask) yield a + 1
    val r1 = Free.run(Reader.run(e1[Reader[Int] :+: Void], 10))
    assert(r1 == 11)
    def e2[U <: Union: Reader[Int]#Member]: Free[U, Int] = Reader.local(e1)(((_: Int) + 10))
    val r2 = Free.run(Reader.run(e2[Reader[Int] :+: Void], 10))
    assert(r2 == 21)
  }

  test("Writer") {
    def e1[U <: Union: Writer[String]#Member]: Free[U, Unit] =
      for {
        _ <- Writer.tell("foo")
        _ <- Writer.tell("bar")
      } yield ()
    val r1 = Free.run(Writer.exec(e1[Writer[String] :+: Void]))
    assert(r1 == "foobar")
    def e2[U <: Union: Writer[String]#Member]: Free[U, Unit] = Writer.listen(e1).flatMap { case (w, _) => Writer.tell(w) }
    val r2 = Free.run(Writer.exec(e2[Writer[String] :+: Void]))
    assert(r2 == "foobarfoobar")
    def e3[U <: Union: Reader[Int]#Member: Writer[Vector[String]]#Member]: Free[U, Int] =
      for {
        _ <- Writer.tell(Vector("begin"))
        n <- Reader.ask
        _ <- Writer.tell(Vector("end"))
      } yield n
    val r3 = Free.run(Writer.run(Reader.run(e3[Reader[Int] :+: Writer[Vector[String]] :+: Void], 10)))
    val r4 = Free.run(Reader.run(Writer.run(e3[Writer[Vector[String]] :+: Reader[Int] :+: Void]), 10))
    assert(r3 == (Vector("begin", "end"), 10))
    assert(r4 == (Vector("begin", "end"), 10))
  }

  test("Error") {
    def e1[U <: Union: Error[Int]#Member](n: Int): Free[U, Int] =
      if (n > 5)
        Error.fail(n)
      else
        Pure(n)
    val r1 = Free.run(Error.run(e1[Error[Int] :+: Void](3)))
    val r2 = Free.run(Error.run(e1[Error[Int] :+: Void](7)))
    assert(r1 == Right(3))
    assert(r2 == Left(7))
  }

  test("State") {
    def e1[U <: Union: State[Int]#Member]: Free[U, Int] =
      for {
        x <- State.get
        _ <- State.put(20)
        y <- State.get
      } yield x + y
    val r1 = Free.run(State.run(e1[State[Int] :+: Void], 10))
    assert(r1 == (20, 30))
    def e2[U <: Union: State[Int]#Member]: Free[U, Int] = State.modify((_: Int) * 2).flatMap(_ => e1)
    val r2 = Free.run(State.run(e2[State[Int] :+: Void], 10))
    assert(r2 == (20, 40))
  }

  test("Choice") {
    def e1[U <: Union: Choice[Vector]#Member]: Free[U, Int] = {
      type F[A] = Free[U, A]
      for {
        n <- Traverse.foldMap(1 to 10)(n => Pure(n): F[Int])
        if n % 2 == 0
      } yield n
    }
    val r1 = Free.run(Choice.run(e1[Choice[Vector] :+: Void]))
    assert(r1 == Vector(2, 4, 6, 8, 10))
  }

}
