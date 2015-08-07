package kits

package spec

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

trait FunctorSpec extends FunSpec with Checkers {

  type F[A]

  type A

  implicit def arbF: Arbitrary[F[A]]

  implicit def arbA: Arbitrary[A]

  val functor: Functor[F]

  import functor._

  describe("Functor") {

    it("identity") {
      check { fa: F[A] =>
        map(fa)(identity) == fa
      }
    }

    it("composition") {
      check { (fa: F[A], f: A => A, g: A => A) =>
        map(map(fa)(f))(g) == map(fa)(f.andThen(g))
      }
    }

  }

}
