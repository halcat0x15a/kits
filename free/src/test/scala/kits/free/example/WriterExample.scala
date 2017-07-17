package kits.free

package example

import org.scalatest.FunSuite

class WriterExample extends FunSuite {

  test("tell") {
    def e1[U: Writer[List[String]]#Member] =
      for {
        _ <- Writer.tell(List("foo"))
        _ <- Writer.tell(List("bar"))
      } yield ()
    val (r1, _) = Writer.handle[List[String]].run(e1)
    assert(r1 == List("foo", "bar"))
  }

  test("listen") {
    def e1[U: Writer[String]#Member] =
      Writer.listen {
        for {
          _ <- Writer.tell("foo")
          _ <- Writer.tell("bar")
        } yield ()
      }.map {
        case (w, _) => w
      }
    val (_, r1) = Writer.handle[String].run(e1)
    assert(r1 == "foobar")
  }

}
