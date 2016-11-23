package kits

package spec

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import scala.collection.immutable.IndexedSeq

class MonoidSpec extends FunSpec with Checkers {

  def law[A: Arbitrary](implicit A: Monoid[A]): Unit = {
    it("rightIdentity") {
      check { a: A =>
        A.append(a, A.empty) == a
      }
    }
    it("leftIdentity") {
      check { a: A =>
        A.append(A.empty, a) == a
      }
    }
    it("associativity") {
      check { (a: A, b: A, c: A) =>
        A.append(A.append(a, b), c) == A.append(a, A.append(b, c))
      }
    }
  }

  describe("Monoid") {
    describe("Conj") {
      implicit val monoid = Monoid.Conj
      law[Boolean]
    }
    describe("Disj") {
      implicit val monoid = Monoid.Disj
      law[Boolean]
    }
    describe("String") {
      law[String]
    }
    describe("Unit") {
      law[Unit]
    }
    describe("Tuple2") {
      law[(String, String)]
    }
    describe("Tuple3") {
      law[(String, String, String)]
    }
    describe("Option") {
      law[Option[String]]
    }
    describe("List") {
      law[List[AnyVal]]
    }
    describe("Vector") {
      law[Vector[AnyVal]]
    }
    describe("IndexedSeq") {
      law[IndexedSeq[AnyVal]]
    }
    describe("Stream") {
      law[Stream[AnyVal]]
    }
    describe("Map") {
      law[Map[AnyVal, String]]
    }
    describe("Set") {
      law[Set[AnyVal]]
    }
    describe("Sum") {
      implicit val monoid = Monoid.Sum[Int]
      law[Int]
    }
    describe("Prod") {
      implicit val monoid = Monoid.Prod[Int]
      law[Int]
    }
  }

}
