package kits.free

package example

import org.scalatest.FunSuite

class WriterExample extends FunSuite {

  test("usage") {
    def e1[U: Writer[String]#Member] =
      for {
        _ <- Writer.tell("foo")
        _ <- Writer.tell("bar")
      } yield ()

    val (r1, _) = Writer.handle[String].run(e1)
    assert(r1 == "foobar")

    val (r2, _) = Writer.handleVec[String].run(e1)
    assert(r2 == Vector("foo", "bar"))

    def e2[U: Writer[String]#Member] =
      Writer.listen(e1).map {
        case (w, _) => w
      }

    val (_, r3) = Writer.handle[String].run(e2)
    assert(r3 == "foobar")
  }

}
