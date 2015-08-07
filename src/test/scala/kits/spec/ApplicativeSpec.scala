package kits

package spec

import org.scalacheck.{Arbitrary, Properties, Prop}

object ApplicativeSpec {

  def apply[F[_], A: Arbitrary](implicit F: Applicative[F], FA: Arbitrary[F[A]]): Properties =
    new Properties("Applicative") {
      property("identity") = Prop.forAll { fa: F[A] =>
        F.ap(fa)(F.pure((a: A) => a)) == fa
      }
      property("composition") = Prop.forAll { (fa: F[A], f: F[A => A], g: F[A => A]) =>
        F.ap(fa)(F.ap(f)(F.ap(g)(F.pure((f: A => A) => (g: A => A) => f.compose(g))))) == F.ap(F.ap(fa)(f))(g)
      }
      property("homomorphism") = Prop.forAll { (a: A, f: A => A) =>
        F.ap(F.pure(a))(F.pure(f)) == F.pure(f(a))
      }
      property("interchange") = Prop.forAll { (a: A, f: F[A => A]) =>
        F.ap(F.pure(a))(f) == F.ap(f)(F.pure((_: A => A)(a)))
      }
    }

}
