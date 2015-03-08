package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

import kits.std._

trait Spec extends FunSpec with Checkers {
  implicit def functor[F[_], A](implicit F: Functor[F], arb: Arbitrary[F[A]]): Arbitrary[F[A => A]] =
    Arbitrary(arb.arbitrary.map(fa => F.map(fa)(a => _ => a)))
}

class StringSpec extends MonoidSpec[String]

class UnitSpec extends MonoidSpec[Unit]

class ListSpec extends MonadPlusSpec[List]

class VectorSpec extends MonadPlusSpec[Vector]

class OptionSpec extends MonadPlusSpec[Option]

class EitherSpec extends MonadSpec[({ type F[A] = Either[AnyVal, A] })#F]
