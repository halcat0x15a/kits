package kits.free

package example

import org.scalatest.FunSuite

class StateExample extends FunSuite {

  test("usage") {
    def e1[U: State[Int]#Member] =
      for {
        n <- State.get
        _ <- State.put(n + 1)
      } yield ()

    val (r1, _) = State.handle(1).run(e1)
    assert(r1 == 2)

    def e2[U: State[Int]#Member] =
      State.modify((_: Int) * 2)

    val (r2, _) = State.handle(2).run(e2)
    assert(r2 == 4)
  }

}
