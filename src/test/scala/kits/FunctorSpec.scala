package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

abstract class FunctorSpec[F[_]](implicit F: Functor[F], arb: Arbitrary[F[AnyVal]]) extends FunSpec with Checkers {
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
