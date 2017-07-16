package kits.free

package example

import org.scalatest.FunSuite
import scala.util.Try

class FreeExample extends FunSuite {

  test("Reader") {
    def e1[U: Reader[Int]#Member] = for (a <- Reader.ask) yield a + 1
    val r1 = Reader.handle(10).run(e1)
    assert(r1 == 11)
    def e2[U: Reader[Int]#Member] = Reader.local(e1)(((_: Int) + 10))
    val r2 = Reader.handle(10).run(e2)
    assert(r2 == 21)
  }

  test("Writer") {
    def e1[U: Writer[String]#Member] =
      for {
        _ <- Writer.tell("foo")
        _ <- Writer.tell("bar")
      } yield ()
    val (r1, _) = Writer.handle[String].run(e1)
    assert(r1 == "foobar")
    def e2[U: Writer[String]#Member] = Writer.listen(e1).flatMap { case (w, _) => Writer.tell(w) }
    val (r2, _) = Writer.handle[String].run(e2)
    assert(r2 == "foobarfoobar")
    def e3[U: Reader[Int]#Member: Writer[Vector[String]]#Member] =
      for {
        _ <- Writer.tell(Vector("foo"))
        n <- Reader.ask
        _ <- Writer.tell(Vector("bar"))
      } yield n
    val r3 = (Reader.handle(10) compose Writer.handle[Vector[String]]).run(e3)
    assert(r3 == (Vector("foo", "bar"), 10))
  }

  test("Error") {
    def e1[U: Error[Int]#Member](n: Int): Free[U, Int] =
      if (n > 5)
        Error.fail(n)
      else
        Pure(n)
    val r1 = Error.handle[Int].run(e1(3))
    assert(r1 == Right(3))
    val r2 = Error.handle[Int].run(e1(7))
    assert(r2 == Left(7))
    def e2[U: Error[Int]#Member: Writer[String]#Member](n: Int) =
      for {
        _ <- Writer.tell("foo")
        n <- e1(n)
        _ <- Writer.tell("bar")
      } yield n
    val r3 = (Writer.handle[String] compose Error.handle[Int]).run(e2(7))
    assert(r3 == ("foo", Left(7)))
    val r4 = (Error.handle[Int] compose Writer.handle[String]).run(e2(7))
    assert(r4 == Left(7))
  }

  test("State") {
    def e1[U: State[Int]#Member] =
      for {
        x <- State.get
        _ <- State.put(20)
        y <- State.get
      } yield x + y
    val r1 = State.handle(10).run(e1)
    assert(r1 == (20, 30))
    def e2[U: State[Int]#Member] = State.modify((_: Int) * 2).flatMap(_ => e1)
    val r2 = State.handle(10).run(e2)
    assert(r2 == (20, 40))
  }

  test("Choice") {
    def e1[U: Choice#Member] =
      for {
        n <- Choice.fromSeq(1 to 10)
        if n % 2 == 0
      } yield n
    val r1 = Choice.handle[Vector].run(e1)
    assert(r1 == Vector(2, 4, 6, 8, 10))
  }

  test("Lift") {
    def e1[U: Writer[String]#Member: Lift[Try]#Member] =
      for {
        x <- Lift.wrap(Try(1))
        _ <- Writer.tell("foo")
        y <- Lift.wrap(Try(2))
      } yield x + y
    val r1 = (Lift.handle[Try] compose Writer.handle[String]).run(e1)
    assert(r1 == Try(("foo", 3)))
  }

  test("Stack safe") {
    def e1[U: State[Int]#Member](n: Int): Free[U, Unit] =
      if (n <= 0)
        Pure(())
      else
        for {
          x <- State.get
          _ <- State.put(x + 1)
          _ <- e1(n - 1)
        } yield ()
    val (r1, _) = State.handle(0).run(e1(10000))
    assert(r1 == 10000)
    def e2[U: Reader[String]#Member: Writer[String]#Member](n: Int): Free[U, Unit] =
      if (n <= 0)
        Pure(())
      else
        for {
          x <- Reader.ask
          _ <- Writer.tell(x)
          _ <- e2(n - 1)
        } yield ()
    val (r2, _) = (Reader.handle("0") compose Writer.handle[String]).run(e2(10000))
    assert(r2.length == 10000)
  }

}
