package kits

package spec

import org.scalacheck.Arbitrary

class MapSpec extends MonoidSpec with FunctorSpec with TraverseSpec {

  type F[A] = Map[AnyVal, A]

  type G[A] = Option[A]

  type T = Map[AnyVal, String]

  type A = AnyVal

  val monoid: Monoid[Map[AnyVal, String]] = implicitly

  val functor: Functor[({ type F[A] = Map[AnyVal, A] })#F] = implicitly

  val traversable: Traverse[({ type F[A] = Map[AnyVal, A] })#F] = implicitly

  val arbF: Arbitrary[Map[AnyVal, AnyVal]] = implicitly

  val arbG: Arbitrary[Option[AnyVal]] = implicitly

  val arbT: Arbitrary[Map[AnyVal, String]] = implicitly

  val arbA: Arbitrary[AnyVal] = implicitly

  val G: Applicative[Option] = implicitly

}
