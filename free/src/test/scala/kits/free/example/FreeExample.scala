package kits.free

package example

import org.scalatest.FunSuite
import scala.util.Try

class FreeExample extends FunSuite {

  test("multiple Reader") {
    def e1[U: Reader[Int]#Member: Reader[String]#Member] =
      for {
        s <- Reader.ask[U, String]
        i <- Reader.ask[U, Int]
      } yield i + s.length
    val r1 = (Reader.handle("hoge") compose Reader.handle(1)).run(e1)
    assert(r1 == 5)

    def e2[U: Reader[Int]#Member: Reader[String]#Member] =
      Reader.local(e1)((_: Int) * 2)
    val r2 = (Reader.handle("fuga") compose Reader.handle(2)).run(e2)
    assert(r2 == 8)
  }

  test("Reader and Writer") {
    def e1[U: Reader[String]#Member: Writer[String]#Member] =
      for {
        s <- Reader.ask
        _ <- Writer.tell(s)
        _ <- Writer.tell(s)
      } yield ()
    val (r1, _) = (Writer.handle[String] compose Reader.handle("hoge")).run(e1)
    assert(r1 == "hogehoge")
  }

  test("Error and State") {
    def e1[U: Error[Int]#Member: State[Int]#Member] =
      for {
        n <- State.get
        _ <- Error.fail(n): Free[U, Int]
      } yield ()
    val r1 = (State.handle(2) compose Error.handle[Int]).run(e1)
    assert(r1 == (2, Left(2)))
    val r2 = (Error.handle[Int] compose State.handle(2)).run(e1)
    assert(r2 == Left(2))
  }

  test("Writer and Lift") {
    def e1[U: Writer[String]#Member: Lift[Try]#Member] =
      for {
        x <- Lift.wrap(Try(1))
        _ <- Writer.tell("hoge")
        y <- Lift.wrap(Try(2))
      } yield x + y
    val r1 = (Lift.handle[Try] compose Writer.handle[String]).run(e1)
    assert(r1 == Try(("hoge", 3)))
  }

  test("stack safe") {
    def e1[U: State[Int]#Member]: Free[U, Int] =
      for {
        i <- State.get
        o <- if (i > 0) {
          for {
            _ <- State.put(i - 1)
            o <- e1
          } yield o + 1
        } else {
          Pure(0): Free[U, Int]
        }
      } yield o
    val (_, r1) = State.handle(100000).run(e1)
    assert(r1 == 100000)
  }

}
