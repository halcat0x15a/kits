package kits

package spec

import org.scalacheck.Arbitrary

class SetSpec extends MonoidSpec with FunctorSpec with ApplicativeSpec with MonadSpec with TraverseSpec {

  type F[A] = Set[A]

  type G[A] = Option[A]

  type T = Set[AnyVal]

  type A = AnyVal

  val monoid: Monoid[Set[AnyVal]] = implicitly

  val functor: Functor[Set] = implicitly

  val applicative: Applicative[Set] = implicitly

  val monad: Monad[Set] = implicitly

  val traversable: Traverse[Set] = implicitly

  val arbF: Arbitrary[Set[AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbT: Arbitrary[Set[AnyVal]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
