package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

abstract class ApplicativeSpec[F[_]](implicit F: Applicative[F], arb: Arbitrary[F[AnyVal]]) extends FunctorSpec[F] {
  implicit def arbFunctor[F[_], A](implicit F: Functor[F], arb: Arbitrary[F[A]]): Arbitrary[F[A => A]] =
    Arbitrary(arb.arbitrary.map(fa => F.map(fa)(a => _ => a)))
  describe("Applicative") {
    it("identity") { fa: F[AnyVal] =>
      F(fa)(F.pure((a: AnyVal) => a)) == fa
    }
    it("composition") {
      check { (fa: F[AnyVal], f: F[AnyVal => AnyVal], g: F[AnyVal => AnyVal]) =>
        F(fa)(F(f)(F(g)(F.pure((f: AnyVal => AnyVal) => (g: AnyVal => AnyVal) => f.compose(g))))) == F(F(fa)(f))(g)
      }
    }
    it("homomorphism") {
      check { (a: AnyVal, f: AnyVal => AnyVal) =>
        F(F.pure(a))(F.pure(f)) == F.pure(f(a))
      }
    }
    it("interchange") {
      check { (a: AnyVal, f: F[AnyVal => AnyVal]) =>
        F(F.pure(a))(f) == F(f)(F.pure((_: AnyVal => AnyVal)(a)))
      }
    }
  }
}

class ConstApplicativeSpec extends ApplicativeSpec[({ type F[A] = Const[String, A] })#F]
