package kits.free

package example

import org.scalatest.FunSuite

class ReaderExample extends FunSuite {

  test("ask") {
    def e1[U: Reader[Int]#Member] =
      for (a <- Reader.ask) yield a + 1
    val r1 = Reader.handle(1).run(e1)
    assert(r1 == 2)
  }

  test("local") {
    def e1[U: Reader[Int]#Member] =
      Reader.local(for (a <- Reader.ask) yield a * 2)((_: Int) + 1)
    val r1 = Reader.handle(1).run(e1)
    assert(r1 == 4)
  }

}
