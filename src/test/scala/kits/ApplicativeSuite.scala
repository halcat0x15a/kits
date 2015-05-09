package kits

import org.scalacheck.Arbitrary

import Applicative.{left, right}

abstract class ApplicativeSuite[F[_], A](implicit F: Applicative[F], FA: Arbitrary[F[A]], A: Arbitrary[A]) extends Suite {

  test("identity") {
    check { fa: F[A] =>
      F(fa)(F.pure((a: A) => a)) == fa
    }
  }

  test("composition") {
    check { (fa: F[A], f: F[A => A], g: F[A => A]) =>
      F(fa)(F(f)(F(g)(F.pure((f: A => A) => (g: A => A) => f.compose(g))))) == F(F(fa)(f))(g)
    }
  }

  test("homomorphism") {
    check { (a: A, f: A => A) =>
      F(F.pure(a))(F.pure(f)) == F.pure(f(a))
    }
  }

  test("interchange") {
    check { (a: A, f: F[A => A]) =>
      F(F.pure(a))(f) == F(f)(F.pure((_: A => A)(a)))
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
