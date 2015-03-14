package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

abstract class Spec[A](implicit val arb: Arbitrary[A]) extends FunSpec with Checkers {
  implicit def arbFunctor[F[_], A](implicit F: Functor[F], arb: Arbitrary[F[A]]): Arbitrary[F[A => A]] =
    Arbitrary(arb.arbitrary.map(fa => F.map(fa)(a => _ => a)))
}

class StringSpec extends MonoidSpec[String] {
  val A = Monoid.string
}

class UnitSpec extends MonoidSpec[Unit] {
  val A = Monoid.unit
}

class ListSpec extends MonadSpec[List] {
  val F = Functor.list
}

class VectorSpec extends MonadSpec[Vector] {
  val F = Functor.vector
}

class OptionSpec extends MonadSpec[Option] {
  val F = Functor.option
}

class EitherMonadSpec extends MonadSpec[({ type F[A] = Either[AnyVal, A] })#F] {
  val F = Functor.right[AnyVal]
}

class SetSpec extends MonadSpec[Set] {
  val F = Functor.set
}
