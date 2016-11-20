package kits

package spec

import org.scalacheck.{Arbitrary, Cogen}
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

class TraverseSpec extends FunSpec with Checkers {

  def law[F[_], G[_], A: Arbitrary: Cogen](implicit F: Traverse[F], G: Applicative[G], FA: Arbitrary[F[A]], GA: Arbitrary[G[A]]): Unit = {
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
      law[Identity, Identity, Int]
    }
    describe("Option") {
      law[Option, Identity, Int]
    }
    describe("Either") {
      law[({ type F[A] = Either[Int, A] })#F, Identity, Int]
    }
    describe("List") {
      law[List, Identity, Int]
    }
    describe("Vector") {
      law[Vector, Identity, Int]
    }
    describe("Map") {
      law[({ type F[A] = Map[Int, A] })#F, Identity, Int]
    }
    describe("Set") {
      law[Set, Identity, Int]
    }
  }

}
