package kits

package spec

import org.scalacheck.Arbitrary

class ValidationSpec extends FunctorSpec with ApplicativeSpec with TraverseSpec {

  type F[A] = Applicative.Validation[String, A]

  type G[A] = Option[A]

  type A = AnyVal

  val functor: Functor[({ type F[A] = Applicative.Validation[String, A] })#F] = implicitly

  val applicative: Applicative[({ type F[A] = Applicative.Validation[String, A] })#F] = implicitly

  val traversable: Traverse[({ type F[A] = Applicative.Validation[String, A] })#F] = implicitly

  val arbF: Arbitrary[Applicative.Validation[String, AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
