package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MonadicSpec[F[_]] extends FunctorSpec[F] {
  implicit val F: Monadic[F]
  implicit val arb: Arbitrary[F[AnyVal]]
  describe("Monadic") {
    it("composition") {
      check { (fa: F[AnyVal], f: AnyVal => F[AnyVal], g: AnyVal => F[AnyVal]) =>
        F.flatMap(fa)(a => F.flatMap(f(a))(g)) == F.flatMap(F.flatMap(fa)(f))(g)
      }
    }
    it("flatten") {
      check { (fa: F[AnyVal], f: AnyVal => F[AnyVal]) =>
        F.flatten(F.map(fa)(f)) == F.flatMap(fa)(f)
      }
    }
  }
}
