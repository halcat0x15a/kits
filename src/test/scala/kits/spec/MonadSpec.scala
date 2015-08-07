package kits

package spec

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

trait MonadSpec extends FunSpec with Checkers {

  type F[A]

  type A

  implicit def arbF: Arbitrary[F[A]]

  implicit def arbA: Arbitrary[A]

  val monad: Monad[F]

  import monad._

  describe("Monad") {

    it("composition") {
      check { (fa: F[A], f: A => F[A], g: A => F[A]) =>
        flatMap(fa)(a => flatMap(f(a))(g)) == flatMap(flatMap(fa)(f))(g)
      }
    }

    it("leftIdentity") {
      check { (a: A, f: A => F[A]) =>
        flatMap(pure(a))(f) == f(a)
      }
    }

    it("rightIdentity") {
      check { fa: F[A] =>
        flatMap(fa)(pure) == fa
      }
    }

  }

}
