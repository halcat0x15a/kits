package kits.free

package example

import org.scalatest.FunSuite
import scala.util.Try

class ErrorExample extends FunSuite {

  test("fail and recover") {
    def e1[U: Error[String]#Member](n: Int): Free[U, Int] =
      if (n >= 0)
        Pure(n)
      else
        Error.fail("hoge")
    val r1 = Error.handle[String].run(e1(2))
    assert(r1 == Right(2))
    val r2 = Error.handle[String].run(e1(-1))
    assert(r2 == Left("hoge"))

    def e2[U: Error[String]#Member](n: Int): Free[U, Int] =
      Error.recover(e1(n)) { (e: String) => Pure(e.length) }
    val r3 = Error.handle[String].run(e2(-1))
    assert(r3 == Right(4))
  }

  test("fromEither") {
    def e1[U: Error[String]#Member]: Free[U, Int] =
      for {
        x <- Error.fromEither(Right(1))
        y <- Error.fromEither(Left("hoge"): Either[String, Int])
      } yield x + y
    val r1 = Error.handle[String].run(e1)
    assert(r1 == Left("hoge"))
  }

  test("fromTry") {
    def e1[U: Error[Throwable]#Member]: Free[U, Int] =
      for {
        x <- Error.fromTry(Try(1))
        y <- Error.fromTry(Try(1 / 0))
      } yield x + y
    val r1 = Error.handle[Throwable].run(e1)
    intercept[ArithmeticException] {
      r1 match {
        case Right(_) => fail()
        case Left(e) => throw e
      }
    }
  }

}
