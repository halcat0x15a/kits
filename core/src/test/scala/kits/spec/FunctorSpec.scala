package kits

package spec

import org.scalacheck.{Arbitrary, Cogen}
import org.scalatest.FunSpec
import org.scalatestplus.scalacheck.Checkers
import scala.collection.immutable.IndexedSeq
import scala.util.Try

class FunctorSpec extends FunSpec with Checkers {

  def law[F[_], A: Arbitrary: Cogen](implicit F: Functor[F], FA: Arbitrary[F[A]]): Unit = {
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
      law[Identity, Int]
    }
    describe("Option") {
      law[Option, Int]
    }
    describe("Either") {
      law[({ type F[A] = Either[Int, A] })#F, Int]
    }
    describe("List") {
      law[List, Int]
    }
    describe("Vector") {
      law[Vector, Int]
    }
    describe("IndexedSeq") {
      law[IndexedSeq, Int]
    }
    describe("Stream") {
      law[Stream, Int]
    }
    describe("Map") {
      law[({ type F[A] = Map[Int, A] })#F, Int]
    }
    describe("Set") {
      law[Set, Int]
    }
    describe("Try") {
      law[Try, Int]
    }
  }

}
