package kits.free

package example

import org.scalatest.FunSuite

class ChoiceExample extends FunSuite {

  test("fromSeq and filter") {
    def e1[U: Choice#Member] =
      for {
        n <- Choice.fromSeq(1 to 10)
        if n % 2 == 0
      } yield n
    val r1 = Choice.handle[Vector].run(e1)
    assert(r1 == Vector(2, 4, 6, 8, 10))
  }

}
