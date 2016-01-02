package kits

package spec

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

class MonoidSpec extends FunSpec with Checkers {

  def law[A: Arbitrary](A: Monoid[A]): Unit = {
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
    describe("Conj")(law(Monoid.conj))
    describe("Disj")(law(Monoid.disj))
    describe("String")(law(Monoid.string))
    describe("Unit")(law(Monoid.unit))
    describe("Pair")(law(Monoid.pair[String, String]))
    describe("Triple")(law(Monoid.triple[String, String, String]))
    describe("Option")(law(Monoid.option[String]))
    describe("First")(Monoid.first[AnyVal])
    describe("Last")(law(Monoid.last[AnyVal]))
    describe("List")(law(Monoid.list[AnyVal]))
    describe("Vector")(law(Monoid.vector[AnyVal]))
    describe("Map")(law(Monoid.map[AnyVal, String]))
    describe("Set")(law(Monoid.set[AnyVal]))
    describe("Sum")(law(Monoid.sum[Int]))
    describe("Prod")(law(Monoid.prod[Int]))
  }

}
