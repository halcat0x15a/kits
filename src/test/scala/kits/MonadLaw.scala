package kits

import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import kits.std._

abstract class MonadLaw[F[_]](implicit F: Monad[F], fa: Arbitrary[F[AnyVal]], tag: ClassTag[F[_]]) extends Properties(s"Monad[${tag.runtimeClass.getSimpleName}]") {
  property("rightIdentity") = forAll { fa: F[AnyVal] =>
    F.flatMap(fa)(F.pure) == fa
  }
  property("leftIdentity") = forAll { (a: AnyVal, f: AnyVal => F[AnyVal]) =>
    F.flatMap(F.pure(a))(f) == f(a)
  }
  property("composition") = forAll { (fa: F[AnyVal], f: AnyVal => F[AnyVal], g: AnyVal => F[AnyVal]) =>
    F.flatMap(fa)(a => F.flatMap(f(a))(g)) == F.flatMap(F.flatMap(fa)(f))(g)
  }
}

object ListMonadSpec extends MonadLaw[List]

object VectorMonadSpec extends MonadLaw[Vector]

object OptionMonadSpec extends MonadLaw[Option]

object EitherMonadSpec extends MonadLaw[({ type F[A] = Either[AnyVal, A] })#F]
