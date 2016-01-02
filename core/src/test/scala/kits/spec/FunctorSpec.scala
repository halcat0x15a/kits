package kits

package spec

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

class FunctorSpec extends FunSpec with Checkers {

  def law[F[_], A: Arbitrary](implicit F: Functor[F], FA: Arbitrary[F[A]]): Unit = {
    it("identity") {
      check { fa: F[A] =>
        F.map(fa)(identity) == fa
      }
    }
    it("composition") {
      check { (fa: F[A], f: A => A, g: A => A) =>
        F.map(F.map(fa)(f))(g) == F.map(fa)(f.andThen(g))
      }
    }
  }

  describe("Functor") {
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
    describe("Map") {
      law[({ type F[A] = Map[AnyVal, A] })#F, AnyVal]
    }
    describe("Set") {
      law[Set, AnyVal]
    }
  }

}
