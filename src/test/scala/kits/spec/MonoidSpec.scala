package kits

package spec

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

trait MonoidSpec extends FunSpec with Checkers {

  type T

  implicit def arbT: Arbitrary[T]

  val monoid: Monoid[T]

  import monoid._

  describe("Monoid") {

    it("rightIdentity") {
      check { a: T =>
        append(a, empty) == a
      }
    }

    it("leftIdentity") {
      check { a: T =>
        append(empty, a) == a
      }
    }

    it("associativity") {
      check { (a: T, b: T, c: T) =>
        append(append(a, b), c) == append(a, append(b, c))
      }
    }

  }

}
