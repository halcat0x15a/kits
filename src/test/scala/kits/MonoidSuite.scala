package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

abstract class MonoidSuite[A: Arbitrary](A: Monoid[A]) extends FunSuite with Checkers {
  test("rightIdentity") {
    check { a: A =>
      A.append(a, A.empty) == a
    }
  }
  test("leftIdentity") {
    check { a: A =>
      A.append(A.empty, a) == a
    }
  }
  test("associativity") {
    check { (a: A, b: A, c: A) =>
      A.append(A.append(a, b), c) == A.append(a, A.append(b, c))
    }
  }
}

class SumMonoidSuite extends MonoidSuite[Int](Monoid.sum)

class ProductMonoidSuite extends MonoidSuite[Int](Monoid.product)

class AllMonoidSuite extends MonoidSuite[Boolean](Monoid.all)

class AnyMonoidSuite extends MonoidSuite[Boolean](Monoid.any)

class StringMonoidSuite extends MonoidSuite[String](Monoid.string)

class UnitMonoidSuite extends MonoidSuite[Unit](Monoid.unit)

class ListMonoidSuite extends MonoidSuite[List[AnyVal]](Monoid.list)

class VectorMonoidSuite extends MonoidSuite[Vector[AnyVal]](Monoid.vector)

class OptionMonoidSuite extends MonoidSuite[Option[String]](Monoid.option)

class FirstMonoidSuite extends MonoidSuite[Option[AnyVal]](Monoid.first)

class LastMonoidSuite extends MonoidSuite[Option[AnyVal]](Monoid.last)

class MapMonoidSuite extends MonoidSuite[Map[AnyVal, String]](Monoid.map)

class SetMonoidSuite extends MonoidSuite[Set[AnyVal]](Monoid.set)

class PairMonoidSuite extends MonoidSuite[(String, String)](Monoid.pair)

class TripleMonoidSuite extends MonoidSuite[(String, String, String)](Monoid.triple)
