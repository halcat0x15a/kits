package kits

package spec

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

trait ApplicativeSpec extends FunSpec with Checkers {

  type F[A]

  type A

  implicit def arbF: Arbitrary[F[A]]

  implicit def arbA: Arbitrary[A]

  implicit val applicative: Applicative[F]

  import applicative._

  describe("Applicative") {

    it("identity") {
      check { fa: F[A] =>
        ap(fa)(pure((a: A) => a)) == fa
      }
    }

    it("composition") {
      check { (fa: F[A], f: F[A => A], g: F[A => A]) =>
        ap(fa)(ap(f)(ap(g)(pure((f: A => A) => (g: A => A) => f.compose(g))))) == ap(ap(fa)(f))(g)
      }
    }

    it("homomorphism") {
      check { (a: A, f: A => A) =>
        ap(pure(a))(pure(f)) == pure(f(a))
      }
    }

    it("interchange") {
      check { (a: A, f: F[A => A]) =>
        ap(pure(a))(f) == ap(f)(pure((_: A => A)(a)))
      }
    }

  }

}
