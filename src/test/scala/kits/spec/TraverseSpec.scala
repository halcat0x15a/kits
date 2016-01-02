package kits

package spec

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

class TraverseSpec extends FunSpec with Checkers {

  def law[F[_], G[_], A: Arbitrary](implicit F: Traverse[F], G: Applicative[G], FA: Arbitrary[F[A]], GA: Arbitrary[G[A]]): Unit = {
    it("identity") {
      check { fa: F[A] =>
        F.traverse[Identity, A, A](fa)(identity) == fa
      }
    }
    it("composition") {
      check { (fa: F[A], f: A => G[A], g: A => G[A]) =>
        F.traverse[({ type H[A] = G[G[A]] })#H, A, A](fa)(f.andThen(G.map(_)(g)))(G.compose(G)) == G.map(F.traverse(fa)(f))(F.traverse(_)(g))
      }
    }
  }

  describe("Traverse") {
    describe("Identity") {
      law[Identity, Identity, AnyVal]
    }
    describe("Option") {
      law[Option, Identity, AnyVal]
    }
    describe("Either") {
      law[({ type F[A] = Either[AnyVal, A] })#F, Identity, AnyVal]
    }
    describe("List") {
      law[List, Identity, AnyVal]
    }
    describe("Vector") {
      law[Vector, Identity, AnyVal]
    }
    describe("Map") {
      law[({ type F[A] = Map[AnyVal, A] })#F, Identity, AnyVal]
    }
    describe("Set") {
      law[Set, Identity, AnyVal]
    }
  }

}
