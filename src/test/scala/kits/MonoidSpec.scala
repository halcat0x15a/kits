package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

abstract class MonoidSpec[A](implicit A: Monoid[A], arb: Arbitrary[A]) extends Spec {
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
