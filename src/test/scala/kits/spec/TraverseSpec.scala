package kits

package spec

import org.scalacheck.{Arbitrary, Properties, Prop}

object TraverseSpec {

  def apply[F[_], G[_], A: Arbitrary](implicit F: Traverse[F], G: Applicative[G], FA: Arbitrary[F[A]], GA: Arbitrary[G[A]]): Properties =
    new Properties("Traverse") {
      property("identity") = Prop.forAll { fa: F[A] =>
        F.traverse[Identity, A, A](fa)(identity) == fa
      }
      property("composition") = Prop.forAll { (fa: F[A], f: A => G[A], g: A => G[A]) =>
        F.traverse[({ type H[A] = G[G[A]] })#H, A, A](fa)(f.andThen(G.map(_)(g)))(G.compose(G)) == G.map(F.traverse(fa)(f))(F.traverse(_)(g))
      }
    }

}
