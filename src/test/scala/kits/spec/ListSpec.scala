package kits

package spec

import org.scalacheck.Arbitrary

class ListSpec extends MonoidSpec with FunctorSpec with ApplicativeSpec with MonadSpec with TraverseSpec {

  type F[A] = List[A]

  type G[A] = Option[A]

  type T = List[AnyVal]

  type A = AnyVal

  val monoid: Monoid[List[AnyVal]] = implicitly

  val functor: Functor[List] = implicitly

  val applicative: Applicative[List] = implicitly

  val monad: Monad[List] = implicitly

  val traversable: Traverse[List] = implicitly

  val arbF: Arbitrary[List[AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbT: Arbitrary[List[AnyVal]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
