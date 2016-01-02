package kits

package spec

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

class MonadSpec extends FunSpec with Checkers {

  def law[F[_], A: Arbitrary](implicit F: Monad[F], FA: Arbitrary[F[A]]): Unit = {
    it("composition") {
      check { (fa: F[A], f: A => F[A], g: A => F[A]) =>
        F.flatMap(fa)(a => F.flatMap(f(a))(g)) == F.flatMap(F.flatMap(fa)(f))(g)
      }
    }
    it("leftIdentity") {
      check { (a: A, f: A => F[A]) =>
        F.flatMap(F.pure(a))(f) == f(a)
      }
    }
    it("rightIdentity") {
      check { fa: F[A] =>
        F.flatMap(fa)(F.pure) == fa
      }
    }
  }

  describe("Monad") {
    describe("Identity") {
      law[Identity, AnyVal]
    }
    describe("Option") {
      law[Option, AnyVal]
    }
    describe("Either") {
      law[({ type F[A] = Either[AnyVal, A] })#F, AnyVal]
    }
    describe("List") {
      law[List, AnyVal]
    }
    describe("Vector") {
      law[Vector, AnyVal]
    }
    describe("Set") {
      law[Set, AnyVal]
    }
  }

}
