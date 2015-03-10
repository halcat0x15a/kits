package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait FunctorSpec[F[_]] extends Spec[F[AnyVal]] {
  implicit val F: Functor[F]
  implicit val arb: Arbitrary[F[AnyVal]]
  describe("Functor") {
    it("identity") {
      check { fa: F[AnyVal] =>
        F.map(fa)(identity) == fa
      }
    }
    it("composition") {
      check { (fa: F[AnyVal], f: AnyVal => AnyVal, g: AnyVal => AnyVal) =>
        F.map(F.map(fa)(f))(g) == F.map(fa)(f.andThen(g))
      }
    }
  }  
}
