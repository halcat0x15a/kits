package kits

import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import kits.std._

abstract class FunctorLaw[F[_]](implicit F: Functor[F], fa: Arbitrary[F[AnyVal]], tag: ClassTag[F[_]]) extends Properties(s"Functor[${tag.runtimeClass.getSimpleName}]") {
  property("identity") = forAll { fa: F[AnyVal] =>
    F.map(fa)(a => a) == fa
  }
  property("composition") = forAll { (fa: F[AnyVal], f: AnyVal => AnyVal, g: AnyVal => AnyVal) =>
    F.map(F.map(fa)(f))(g) == F.map(fa)(f.andThen(g))
  }  
}

object ListFunctorSpec extends FunctorLaw[List]

object VectorFunctorSpec extends FunctorLaw[Vector]

object OptionFunctorSpec extends FunctorLaw[Option]

object EitherFunctorSpec extends FunctorLaw[({ type F[A] = Either[AnyVal, A] })#F]
