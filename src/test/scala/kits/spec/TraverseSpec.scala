package kits

package spec

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

trait TraverseSpec extends FunSpec with Checkers {

  type F[A]

  type G[A]

  type A

  implicit def G: Applicative[G]

  implicit def arbF: Arbitrary[F[A]]

  implicit def arbG: Arbitrary[G[A]]

  val traversable: Traverse[F]

  import traversable._

  describe("Traverse") {

    it("identity") {
      check { fa: F[A] =>
        traverse[Identity, A, A](fa)(identity) == fa
      }
    }

    it("composition") {
      check { (fa: F[A], f: A => G[A], g: A => G[A]) =>
        traverse[({ type H[A] = G[G[A]] })#H, A, A](fa)(f.andThen(G.map(_)(g)))(G.compose(G)) == G.map(traverse(fa)(f))(traverse(_)(g))
      }
    }

  }

}
