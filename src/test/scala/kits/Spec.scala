package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

import std._

abstract class Spec[A](implicit val arb: Arbitrary[A]) extends FunSpec with Checkers {
  implicit def arbFunctor[F[_], A](implicit F: Functor[F], arb: Arbitrary[F[A]]): Arbitrary[F[A => A]] =
    Arbitrary(arb.arbitrary.map(fa => F.map(fa)(a => _ => a)))
}

class StringSpec extends MonoidSpec[String] {
  val A = std.string
}

class UnitSpec extends MonoidSpec[Unit] {
  val A = std.unit
}

class ListSpec extends MonadicPlusSpec[List] with ApplicativeSpec[List] {
  val F = std.list
}

class VectorSpec extends MonadicPlusSpec[Vector] with ApplicativeSpec[Vector] {
  val F = std.vector
}

class OptionSpec extends MonadicPlusSpec[Option] with ApplicativeSpec[Option] {
  val F = std.option
}

class EitherMonadSpec extends MonadicSpec[({ type F[A] = Either[AnyVal, A] })#F] with ApplicativeSpec[({ type F[A] = Either[AnyVal, A] })#F] {
  val F = std.either[AnyVal]
}

class MapSpec extends MonadicPlusSpec[({ type F[A] = Map[AnyVal, A] })#F] {
  val F = std.map[AnyVal]
}
