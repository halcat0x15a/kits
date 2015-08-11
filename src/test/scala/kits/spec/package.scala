package kits

import org.scalacheck.{Arbitrary, Gen}

package object spec {

  implicit def arbSum[A: Numeric](implicit A: Arbitrary[A]): Arbitrary[Sum[A]] = Arbitrary(A.arbitrary.map(Sum[A]))

  implicit def arbProd[A: Numeric](implicit A: Arbitrary[A]): Arbitrary[Prod[A]] = Arbitrary(A.arbitrary.map(Prod[A]))

  implicit def arbConj: Arbitrary[Conj] = Arbitrary(Arbitrary.arbBool.arbitrary.map(Conj))

  implicit def arbDisj: Arbitrary[Disj] = Arbitrary(Arbitrary.arbBool.arbitrary.map(Disj))

  implicit def arbFirst[A](implicit A: Arbitrary[A]): Arbitrary[First[A]] = Arbitrary(Gen.option(A.arbitrary).map(First[A]))

  implicit def arbLast[A](implicit A: Arbitrary[A]): Arbitrary[Last[A]] = Arbitrary(Gen.option(A.arbitrary).map(Last[A]))

  implicit def arbValidation[E: Arbitrary, A: Arbitrary]: Arbitrary[Validation[E, A]] = Arbitrary(Arbitrary.arbEither[E, A].arbitrary.map(Validation[E, A]))

  implicit def arbApplicative[F[_], A](implicit F: Applicative[F], A: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(A.arbitrary.map(F.pure))

}
