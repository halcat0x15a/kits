package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

abstract class MonadSpec[F[_]](implicit F: Monad[F], arb: Arbitrary[F[AnyVal]]) extends ApplicativeSpec[F] {
  describe("Monad") {
    it("rightIdentity") {
      check { fa: F[AnyVal] =>
        F.flatMap(fa)(F.pure) == fa
      }
    }
    it("leftIdentity") {
      check { (a: AnyVal, f: AnyVal => F[AnyVal]) =>
        F.flatMap(F.pure(a))(f) == f(a)
      }
    }
    it("composition") {
      check { (fa: F[AnyVal], f: AnyVal => F[AnyVal], g: AnyVal => F[AnyVal]) =>
        F.flatMap(fa)(a => F.flatMap(f(a))(g)) == F.flatMap(F.flatMap(fa)(f))(g)
      }
    }
  }
}
