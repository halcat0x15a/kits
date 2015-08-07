package kits

package spec

import org.scalacheck.Arbitrary

class EitherSpec extends FunctorSpec with ApplicativeSpec with MonadSpec with TraverseSpec {

  type F[A] = Either[AnyVal, A]

  type G[A] = Option[A]

  type A = AnyVal

  val functor: Functor[({ type F[A] = Either[AnyVal, A] })#F] = implicitly

  val applicative: Applicative[({ type F[A] = Either[AnyVal, A] })#F] = implicitly

  val monad: Monad[({ type F[A] = Either[AnyVal, A] })#F] = implicitly

  val traversable: Traverse[({ type F[A] = Either[AnyVal, A] })#F] = implicitly

  val arbF: Arbitrary[Either[AnyVal, AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
