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
    it("associativity") {
      forAll { (a: A, b: A, c: A) =>
        A.append(A.append(a, b), c) == A.append(a, A.append(b, c))
      }
    }
  }
}
