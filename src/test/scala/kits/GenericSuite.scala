package kits

import org.scalacheck.Arbitrary

import Monoid.product

case class Rational(n: Int, d: Int)

object Rational {

  implicit val generic = Generic.derive[Rational]

  implicit val arbitrary = Arbitrary(
    for {
      n <- Arbitrary.arbitrary[Int]
      d <- Arbitrary.arbitrary[Int]
    } yield Rational(n, d)
  )

}

class RationalMonoidSuite extends MonoidSuite[Rational](Monoid.generic)
