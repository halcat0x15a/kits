package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MonoidSpec[A] extends Spec[A] {
  implicit val A: Monoid[A]
  describe("Monoid") {
    it("rightIdentity") {
      check { a: A =>
        A.append(a, A.zero) == a
      }
    }
    it("leftIdentity") {
      forAll { a: A =>
        A.append(A.zero, a) == a
      }
    }
  }
}
