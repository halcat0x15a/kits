package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

abstract class MonoidSpec[A](implicit A: Monoid[A], arb: Arbitrary[A]) extends FunSpec with Checkers {
  describe("Monoid") {
    it("rightIdentity") {
      check { a: A =>
        A.append(a, A.zero) == a
      }
    }
    it("leftIdentity") {
      forAll { a: A =>
        A.append(A.zero, a) == a
      }
    }
    it("associativity") {
      forAll { (a: A, b: A, c: A) =>
        A.append(A.append(a, b), c) == A.append(a, A.append(b, c))
      }
    }
  }
}

class StringMonoidSpec extends MonoidSpec[String]

class UnitMonoidSpec extends MonoidSpec[Unit]

class ListMonoidSpec extends MonoidSpec[List[AnyVal]]

class VectorMonoidSpec extends MonoidSpec[Vector[AnyVal]]

class OptionMonoidSpec extends MonoidSpec[Option[String]]

class MapMonoidSpec extends MonoidSpec[Map[AnyVal, String]]

class SetMonoidSpec extends MonoidSpec[Set[AnyVal]]
