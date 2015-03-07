package kits

import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import kits.std._

abstract class MonadPlusLaw[F[_]](implicit F: MonadPlus[F], fa: Arbitrary[F[AnyVal]], tag: ClassTag[F[_]]) extends Properties(s"MonadPlus[${tag.runtimeClass.getSimpleName}]") {
  property("rightIdentity") = forAll { a: F[AnyVal] =>
    F.plus(a, F.empty) == a
  }
  property("leftIdentity") = forAll { a: F[AnyVal] =>
    F.plus(F.empty, a) == a
  }
}

object ListMonadPlusSpec extends MonadPlusLaw[List]

object VectorMonadPlusSpec extends MonadPlusLaw[Vector]

object OptionMonadPlusSpec extends MonadPlusLaw[Option]
