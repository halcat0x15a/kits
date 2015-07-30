package kits

import org.scalacheck.{Arbitrary, Gen}

package object test {

  implicit def arbSum[A: Numeric](implicit A: Arbitrary[A]): Arbitrary[Monoid.Sum[A]] = Arbitrary(A.arbitrary.map(Monoid.Sum[A]))

  implicit def arbProduct[A: Numeric](implicit A: Arbitrary[A]): Arbitrary[Monoid.Product[A]] = Arbitrary(A.arbitrary.map(Monoid.Product[A]))

  implicit def arbAll: Arbitrary[Monoid.All] = Arbitrary(Arbitrary.arbBool.arbitrary.map(Monoid.All))

  implicit def arbAny: Arbitrary[Monoid.Any] = Arbitrary(Arbitrary.arbBool.arbitrary.map(Monoid.Any))

  implicit def arbFirst[A](implicit A: Arbitrary[A]): Arbitrary[Monoid.First[A]] = Arbitrary(Gen.option(A.arbitrary).map(Monoid.First[A]))

  implicit def arbLast[A](implicit A: Arbitrary[A]): Arbitrary[Monoid.Last[A]] = Arbitrary(Gen.option(A.arbitrary).map(Monoid.Last[A]))

  implicit def arbValidation[E: Arbitrary, A: Arbitrary]: Arbitrary[Applicative.Validation[E, A]] = Arbitrary(Arbitrary.arbEither[E, A].arbitrary.map(Applicative.Validation[E, A]))

  implicit def arbApplicative[F[_], A](implicit F: Applicative[F], A: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(A.arbitrary.map(F.pure))

}
