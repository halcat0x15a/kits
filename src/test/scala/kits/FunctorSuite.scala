package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

abstract class FunctorSuite[F[_], A](implicit F: Functor[F], FA: Arbitrary[F[A]], A: Arbitrary[A]) extends FunSuite with Checkers {
  test("identity") {
    check { fa: F[A] =>
      F.map(fa)(identity) == fa
    }
  }
  test("composition") {
    check { (fa: F[A], f: A => A, g: A => A) =>
      F.map(F.map(fa)(f))(g) == F.map(fa)(f.andThen(g))
    }
  }
}

class IdFunctorSuite extends FunctorSuite[Id, AnyVal]

class ListFunctorSuite extends FunctorSuite[List, AnyVal]

class VectorFunctorSuite extends FunctorSuite[Vector, AnyVal]

class OptionFunctorSuite extends FunctorSuite[Option, AnyVal]

class MapFunctorSuite extends FunctorSuite[({ type F[A] = Map[AnyVal, A] })#F, AnyVal]

class SetFunctorSuite extends FunctorSuite[Set, AnyVal]

class RightFunctorSuite extends FunctorSuite[({ type F[A] = Either[AnyVal, A] })#F, AnyVal]

class LeftFunctorSuite extends FunctorSuite[({ type F[A] = Either[A, AnyVal] })#F, AnyVal]
