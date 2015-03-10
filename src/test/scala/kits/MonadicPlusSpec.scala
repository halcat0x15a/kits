package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MonadicPlusSpec[F[_]] extends MonadicSpec[F] {
  implicit val F: MonadicPlus[F]
  implicit val arb: Arbitrary[F[AnyVal]]
  describe("MonadicPlus") {
    it("rightIdentity") {
      check { fa: F[AnyVal] =>
        F.plus(fa, F.empty) == fa
      }
    }
    it("leftIdentity") {
      forAll { fa: F[AnyVal] =>
        F.plus(F.empty, fa) == fa
      }
    }
    it("falsity") {
      forAll { fa: F[AnyVal] =>
        F.filter(fa)(_ => false) == F.empty
      }
    }
  }
}
