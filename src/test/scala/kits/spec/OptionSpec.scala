package kits

package spec

import org.scalacheck.Arbitrary

class OptionSpec extends MonoidSpec with FunctorSpec with ApplicativeSpec with MonadSpec with TraverseSpec {

  type F[A] = Option[A]

  type G[A] = Option[A]

  type T = Option[String]

  type A = AnyVal

  val monoid: Monoid[Option[String]] = implicitly

  val functor: Functor[Option] = implicitly

  val applicative: Applicative[Option] = implicitly

  val monad: Monad[Option] = implicitly

  val traversable: Traverse[Option] = implicitly

  val arbF: Arbitrary[Option[AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbT: Arbitrary[Option[String]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
