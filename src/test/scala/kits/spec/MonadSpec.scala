package kits

package spec

import org.scalacheck.{Arbitrary, Properties, Prop}

object MonadSpec {

  def apply[F[_], A: Arbitrary](implicit F: Monad[F], FA: Arbitrary[F[A]]): Properties =
    new Properties("Monad") {
      property("composition") = Prop.forAll { (fa: F[A], f: A => F[A], g: A => F[A]) =>
        F.flatMap(fa)(a => F.flatMap(f(a))(g)) == F.flatMap(F.flatMap(fa)(f))(g)
      }
      property("leftIdentity") = Prop.forAll { (a: A, f: A => F[A]) =>
        F.flatMap(F.pure(a))(f) == f(a)
      }
      property("rightIdentity") = Prop.forAll { fa: F[A] =>
        F.flatMap(fa)(F.pure) == fa
      }
    }

}
