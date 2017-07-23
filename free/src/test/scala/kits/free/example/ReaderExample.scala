package kits.free

package example

import org.scalatest.FunSuite

class ReaderExample extends FunSuite {

  test("usage") {
    def e1[U: Reader[Int]#Member] =
      for (a <- Reader.ask) yield a * 2

    val r1 = Reader.handle(1).run(e1)
    assert(r1 == 2)

    def e2[U: Reader[Int]#Member] =
      Reader.local(e1)((_: Int) + 1)

    val r2 = Reader.handle(1).run(e2)
    assert(r2 == 4)
  }

}
