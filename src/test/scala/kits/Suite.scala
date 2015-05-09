package kits

import org.scalatest.FunSuite

import org.scalatest.prop.Checkers

import org.scalacheck.Arbitrary

abstract class Suite extends FunSuite with Checkers {

  implicit def applicative[F[_], A](implicit F: Applicative[F], A: Arbitrary[A]): Arbitrary[F[A]] = Arbitrary(A.arbitrary.map(F.pure))

}
