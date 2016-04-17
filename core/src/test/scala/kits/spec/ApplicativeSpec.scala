package kits

package spec

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

class ApplicativeSpec extends FunSpec with Checkers {

  def law[F[_], A: Arbitrary](implicit F: Applicative[F], FA: Arbitrary[F[A]]): Unit = {
    it("identity") {
      check { fa: F[A] =>
        F.ap(fa)(F.pure((a: A) => a)) == fa
      }
    }
    it("composition") {
      check { (fa: F[A], f: A => A, g: A => A) =>
        F.ap(fa)(F.ap(F.pure(f))(F.ap(F.pure(g))(F.pure((f: A => A) => (g: A => A) => f.compose(g))))) == F.ap(F.ap(fa)(F.pure(f)))(F.pure(g))
      }
    }
    it("homomorphism") {
      check { (a: A, f: A => A) =>
        F.ap(F.pure(a))(F.pure(f)) == F.pure(f(a))
      }
    }
    it("interchange") {
      check { (a: A, f: A => A) =>
        F.ap(F.pure(a))(F.pure(f)) == F.ap(F.pure(f))(F.pure((_: A => A)(a)))
      }
    }
  }

  describe("Applicative") {
    describe("Identity") {
      law[Identity, AnyVal]
    }
    describe("Option") {
      law[Option, AnyVal]
    }
    describe("Either") {
      law[({ type F[A] = Either[String, A] })#F, AnyVal]
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
