package kits

import org.scalacheck.{Arbitrary, Gen}

package object spec {

  implicit def arbSum[A: Numeric](implicit A: Arbitrary[A]): Arbitrary[Sum[A]] = Arbitrary(A.arbitrary.map(Sum[A]))

  implicit def arbProduct[A: Numeric](implicit A: Arbitrary[A]): Arbitrary[Product[A]] = Arbitrary(A.arbitrary.map(Product[A]))

  implicit def arbAll: Arbitrary[All] = Arbitrary(Arbitrary.arbBool.arbitrary.map(All))

  implicit def arbAny: Arbitrary[Any] = Arbitrary(Arbitrary.arbBool.arbitrary.map(Any))

  implicit def arbFirst[A](implicit A: Arbitrary[A]): Arbitrary[First[A]] = Arbitrary(Gen.option(A.arbitrary).map(First[A]))

  implicit def arbLast[A](implicit A: Arbitrary[A]): Arbitrary[Last[A]] = Arbitrary(Gen.option(A.arbitrary).map(Last[A]))

  implicit def arbValidation[E: Arbitrary, A: Arbitrary]: Arbitrary[Validation[E, A]] = Arbitrary(Arbitrary.arbEither[E, A].arbitrary.map(Validation[E, A]))

  implicit def arbApplicative[F[_], A](implicit F: Applicative[F], A: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(A.arbitrary.map(F.pure))

}
