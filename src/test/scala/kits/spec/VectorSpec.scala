package kits

package spec

import org.scalacheck.Arbitrary

class VectorSpec extends MonoidSpec with FunctorSpec with ApplicativeSpec with MonadSpec with TraverseSpec {

  type F[A] = Vector[A]

  type G[A] = Option[A]

  type T = Vector[AnyVal]

  type A = AnyVal

  val monoid: Monoid[Vector[AnyVal]] = implicitly

  val functor: Functor[Vector] = implicitly

  val applicative: Applicative[Vector] = implicitly

  val monad: Monad[Vector] = implicitly

  val traversable: Traverse[Vector] = implicitly

  val arbF: Arbitrary[Vector[AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbT: Arbitrary[Vector[AnyVal]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
