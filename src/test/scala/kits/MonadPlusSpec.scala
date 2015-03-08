package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

abstract class MonadPlusSpec[F[_]](implicit F: MonadPlus[F], arb: Arbitrary[F[AnyVal]]) extends MonadSpec[F] {
  describe("MonadPlus") {
    it("rightIdentity") {
      check { a: F[AnyVal] =>
        F.plus(a, F.empty) == a
      }
    }
    it("leftIdentity") {
      forAll { a: F[AnyVal] =>
        F.plus(F.empty, a) == a
      }
    }
  }
}
