package kits

package spec

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

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
      import kits.Monoid.Conj
      law[Boolean]
    }
    describe("Disj") {
      import kits.Monoid.Disj
      law[Boolean]
    }
    describe("String") {
      law[String]
    }
    describe("Unit") {
      law[Unit]
    }
    describe("Pair") {
      law[(String, String)]
    }
    describe("Triple") {
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
    describe("Map") {
      law[Map[AnyVal, String]]
    }
    describe("Set") {
      law[Set[AnyVal]]
    }
    describe("Sum") {
      import kits.Monoid.Sum
      law[Int]
    }
    describe("Prod") {
      import kits.Monoid.Prod
      law[Int]
    }
  }

}
