package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait MonadSpec[F[_]] extends ApplicativeSpec[F] {
  implicit val F: Monad[F]
  implicit val arb: Arbitrary[F[AnyVal]]
  describe("Monad") {
    it("composition") {
      check { (fa: F[AnyVal], f: AnyVal => F[AnyVal], g: AnyVal => F[AnyVal]) =>
        F.flatMap(fa)(a => F.flatMap(f(a))(g)) == F.flatMap(F.flatMap(fa)(f))(g)
      }
    }
    it("leftIdentity") {
      check { (a: AnyVal, f: AnyVal => F[AnyVal]) =>
        F.flatMap(F.pure(a))(f) == f(a)
      }
    }
    it("rightIdentity") {
      check { fa: F[AnyVal] =>
        F.flatMap(fa)(F.pure) == fa
      }
    }
  }
}
