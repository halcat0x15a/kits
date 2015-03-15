package kits

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

abstract class MonoidSpec[A](A: Monoid[A])(implicit arb: Arbitrary[A]) extends FunSpec with Checkers {
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

class SumMonoidSpec extends MonoidSpec[Int](Monoid.sum)

class ProductMonoidSpec extends MonoidSpec[Int](Monoid.product)

class AllMonoidSpec extends MonoidSpec[Boolean](Monoid.all)

class AnyMonoidSpec extends MonoidSpec[Boolean](Monoid.any)

class StringMonoidSpec extends MonoidSpec[String](Monoid.string)

class UnitMonoidSpec extends MonoidSpec[Unit](Monoid.unit)

class ListMonoidSpec extends MonoidSpec[List[AnyVal]](Monoid.list)

class VectorMonoidSpec extends MonoidSpec[Vector[AnyVal]](Monoid.vector)

class OptionMonoidSpec extends MonoidSpec[Option[String]](Monoid.option)

class FirstMonoidSpec extends MonoidSpec[Option[AnyVal]](Monoid.first)

class LastMonoidSpec extends MonoidSpec[Option[AnyVal]](Monoid.last)

class MapMonoidSpec extends MonoidSpec[Map[AnyVal, String]](Monoid.map)

class SetMonoidSpec extends MonoidSpec[Set[AnyVal]](Monoid.set)
