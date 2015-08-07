package kits

package spec

import org.scalacheck.Arbitrary

class IdentitySpec extends FunctorSpec with ApplicativeSpec with MonadSpec with TraverseSpec {

  type F[A] = Identity[A]

  type G[A] = Option[A]

  type A = AnyVal

  val functor: Functor[Identity] = implicitly

  val applicative: Applicative[Identity] = implicitly

  val monad: Monad[Identity] = implicitly

  val traversable: Traverse[Identity] = implicitly

  val arbF: Arbitrary[Identity[AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
