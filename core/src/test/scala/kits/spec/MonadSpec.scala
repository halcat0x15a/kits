package kits

package spec

import org.scalacheck.{Arbitrary, Cogen}
import org.scalatest.FunSpec
import org.scalatestplus.scalacheck.Checkers
import scala.collection.immutable.IndexedSeq
import scala.util.Try

class MonadSpec extends FunSpec with Checkers {

  def law[F[_], A: Arbitrary: Cogen](implicit F: Monad[F], FA: Arbitrary[F[A]]): Unit = {
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
    describe("Set") {
      law[Set, Int]
    }
    describe("Try") {
      law[Try, Int]
    }
  }

}
