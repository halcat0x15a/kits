package kits

package spec

import org.scalacheck.{Arbitrary, Properties, Prop}

object FunctorSpec {

  def apply[F[_], A: Arbitrary](implicit F: Functor[F], FA: Arbitrary[F[A]]): Properties =
    new Properties("Functor") {
      property("identity") = Prop.forAll { fa: F[A] =>
        F.map(fa)(identity) == fa
      }
      property("composition") = Prop.forAll { (fa: F[A], f: A => A, g: A => A) =>
        F.map(F.map(fa)(f))(g) == F.map(fa)(f.andThen(g))
      }
    }

}
