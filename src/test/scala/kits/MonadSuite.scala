package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

abstract class MonadSuite[F[_], A](implicit F: Monad[F], FA: Arbitrary[F[A]], A: Arbitrary[A]) extends FunSuite with Checkers {
  test("composition") {
    check { (fa: F[A], f: A => F[A], g: A => F[A]) =>
      F.flatMap(fa)(a => F.flatMap(f(a))(g)) == F.flatMap(F.flatMap(fa)(f))(g)
    }
  }
  test("leftIdentity") {
    check { (a: A, f: A => F[A]) =>
      F.flatMap(F.pure(a))(f) == f(a)
    }
  }
  test("rightIdentity") {
    check { fa: F[A] =>
      F.flatMap(fa)(F.pure) == fa
    }
  }
}

class IdMonadSuite extends MonadSuite[Id, AnyVal]

class ListMonadSuite extends MonadSuite[List, AnyVal]

class VectorMonadSuite extends MonadSuite[Vector, AnyVal]

class OptionMonadSuite extends MonadSuite[Option, AnyVal]

class SetMonadSuite extends MonadSuite[Set, AnyVal]

class RightMonadSuite extends MonadSuite[({ type F[A] = Either[AnyVal, A] })#F, AnyVal]

class LeftMonadSuite extends MonadSuite[({ type F[A] = Either[A, AnyVal] })#F, AnyVal]
