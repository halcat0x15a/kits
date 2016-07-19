package kits

package free

package example

import org.scalatest.FunSuite

class FreeExample extends FunSuite {

  test("Reader") {
    def e1[U <: Union: Reader[Int]#Member]: Free[U, Int] = for (a <- Reader.ask) yield a + 1
    val r1 = Reader.run(10)(e1)
    assert(r1 == 11)
    def e2[U <: Union: Reader[Int]#Member]: Free[U, Int] = Reader.local(e1)(((_: Int) + 10))
    val r2 = Reader.run(10)(e2)
    assert(r2 == 21)
  }

  test("Writer") {
    def e1[U <: Union: Writer[String]#Member]: Free[U, Unit] =
      for {
        _ <- Writer.tell("foo")
        _ <- Writer.tell("bar")
      } yield ()
    val (r1, _) = Writer.run[String].apply(e1)
    assert(r1 == "foobar")
    def e2[U <: Union: Writer[String]#Member]: Free[U, Unit] = Writer.listen(e1).flatMap { case (w, _) => Writer.tell(w) }
    val (r2, _) = Writer.run[String].apply(e2)
    assert(r2 == "foobarfoobar")
    def e3[U <: Union: Reader[Int]#Member: Writer[Vector[String]]#Member]: Free[U, Int] =
      for {
        _ <- Writer.tell(Vector("begin"))
        n <- Reader.ask
        _ <- Writer.tell(Vector("end"))
      } yield n
    val r3 = (Writer.run[Vector[String]] compose Reader.run(10))(e3)
    val r4 = (Reader.run(10) compose Writer.run[Vector[String]])(e3)
    assert(r3 == (Vector("begin", "end"), 10))
    assert(r4 == (Vector("begin", "end"), 10))
  }

  test("Error") {
    def e1[U <: Union: Error[Int]#Member](n: Int): Free[U, Int] =
      if (n > 5)
        Error.fail(n)
      else
        Pure(n)
    val r1 = Error.run[Int](e1(3))
    val r2 = Error.run[Int](e1(7))
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
    val r1 = State.run(10)(e1)
    assert(r1 == (20, 30))
    def e2[U <: Union: State[Int]#Member]: Free[U, Int] = State.modify((_: Int) * 2).flatMap(_ => e1)
    val r2 = State.run(10)(e2)
    assert(r2 == (20, 40))
  }

  test("Choice") {
    import kits.Traverse.Implicits._
    def e1[U <: Union: Choice[Vector]#Member]: Free[U, Int] =
      for {
        n <- (1 to 10).toIndexedSeq.foldMap(n => Pure(n): Free[U, Int])
        if n % 2 == 0
      } yield n
    val r1 = Choice.run[Vector].apply(e1)
    assert(r1 == Vector(2, 4, 6, 8, 10))
  }

  test("Stack safe") {
    def e1[U <: Union: State[Int]#Member](n: Int): Free[U, Unit] =
      if (n <= 0)
        Pure(())
      else
        for {
          x <- State.get
          _ <- State.put(x + 1)
          _ <- e1(n - 1)
        } yield ()
    val (r1, _) = State.run(0)(e1(10000))
    assert(r1 == 10000)
    def e2[U <: Union: Reader[String]#Member: Writer[String]#Member](n: Int): Free[U, Unit] =
      if (n <= 0)
        Pure(())
      else
        for {
          x <- Reader.ask
          _ <- Writer.tell(x)
          _ <- e2(n - 1)
        } yield ()
    val (r2, _) = (Reader.run("0") compose Writer.run[String])(e2(10000))
    assert(r2.length == 10000)
  }

}
