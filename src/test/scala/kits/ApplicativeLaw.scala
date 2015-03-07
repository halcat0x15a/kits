package kits

import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import kits.std._

abstract class ApplicativeLaw[F[_]](implicit F: Applicative[F], fa: Arbitrary[F[AnyVal]], f: Arbitrary[F[AnyVal => AnyVal]], tag: ClassTag[F[_]]) extends Properties(s"Applicative[${tag.runtimeClass.getSimpleName}]") {
  property("identity") = forAll { fa: F[AnyVal] =>
    F(fa)(F.pure((a: AnyVal) => a)) == fa
  }
  property("composition") = forAll { (fa: F[AnyVal], f: F[AnyVal => AnyVal], g: F[AnyVal => AnyVal]) =>
    F(fa)(F(f)(F(g)(F.pure((f: AnyVal => AnyVal) => (g: AnyVal => AnyVal) => f.compose(g))))) == F(F(fa)(f))(g)
  }
  property("homomorphism") = forAll { (a: AnyVal, f: AnyVal => AnyVal) =>
    F(F.pure(a))(F.pure(f)) == F.pure(f(a))
  }
  property("interchange") = forAll { (a: AnyVal, f: F[AnyVal => AnyVal]) =>
    F(F.pure(a))(f) == F(f)(F.pure((_: AnyVal => AnyVal)(a)))
  }
}

object ListApplicativeSpec extends ApplicativeLaw[List]

object VectorApplicativeSpec extends ApplicativeLaw[Vector]

object OptionApplicativeSpec extends ApplicativeLaw[Option]

object EitherApplicativeSpec extends ApplicativeLaw[({ type F[A] = Either[AnyVal, A] })#F]
