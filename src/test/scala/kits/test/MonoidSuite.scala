package kits

package test

import org.scalacheck.Arbitrary

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

abstract class MonoidSuite[A: Arbitrary](implicit A: Monoid[A]) extends FunSuite with Checkers {

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

class SumMonoidSuite extends MonoidSuite[Monoid.Sum[Int]]

class ProductMonoidSuite extends MonoidSuite[Monoid.Product[Int]]

class AllMonoidSuite extends MonoidSuite[Monoid.All]

class AnyMonoidSuite extends MonoidSuite[Monoid.Any]

class StringMonoidSuite extends MonoidSuite[String]

class UnitMonoidSuite extends MonoidSuite[Unit]

class ListMonoidSuite extends MonoidSuite[List[AnyVal]]

class VectorMonoidSuite extends MonoidSuite[Vector[AnyVal]]

class OptionMonoidSuite extends MonoidSuite[Option[String]]

class FirstMonoidSuite extends MonoidSuite[Monoid.First[AnyVal]]

class LastMonoidSuite extends MonoidSuite[Monoid.Last[AnyVal]]

class MapMonoidSuite extends MonoidSuite[Map[AnyVal, String]]

class SetMonoidSuite extends MonoidSuite[Set[AnyVal]]

class PairMonoidSuite extends MonoidSuite[(String, String)]

class TripleMonoidSuite extends MonoidSuite[(String, String, String)]
