package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import Applicative.{left, right}

abstract class ApplicativeSuite[F[_], A](implicit F: Applicative[F], FA: Arbitrary[F[A]], A: Arbitrary[A]) extends FunSuite with Checkers {
  implicit def pure[A](implicit A: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(A.arbitrary.map(F.pure))
  test("identity") {
    check { fa: F[A] =>
      F.ap(fa)(F.pure((a: A) => a)) == fa
    }
  }
  test("composition") {
    check { (fa: F[A], f: F[A => A], g: F[A => A]) =>
      F.ap(fa)(F.ap(f)(F.ap(g)(F.pure((f: A => A) => (g: A => A) => f.compose(g))))) == F.ap(F.ap(fa)(f))(g)
    }
  }
  test("homomorphism") {
    check { (a: A, f: A => A) =>
      F.ap(F.pure(a))(F.pure(f)) == F.pure(f(a))
    }
  }
  test("interchange") {
    check { (a: A, f: F[A => A]) =>
      F.ap(F.pure(a))(f) == F.ap(f)(F.pure((_: A => A)(a)))
    }
  }
}

class IdentityApplicativeSuite extends ApplicativeSuite[Identity, AnyVal]

class ListApplicativeSuite extends ApplicativeSuite[List, AnyVal]

class VectorApplicativeSuite extends ApplicativeSuite[Vector, AnyVal]

class OptionApplicativeSuite extends ApplicativeSuite[Option, AnyVal]

class SetApplicativeSuite extends ApplicativeSuite[Set, AnyVal]

class RightApplicativeSuite extends ApplicativeSuite[({ type F[A] = Either[String, A] })#F, AnyVal]

class LeftApplicativeSuite extends ApplicativeSuite[({ type F[A] = Either[A, String] })#F, AnyVal]
