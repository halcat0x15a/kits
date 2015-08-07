package kits

package spec

import org.scalacheck.{Arbitrary, Properties, Prop}

object MonoidSpec {

  def apply[A: Arbitrary](implicit A: Monoid[A]): Properties =
    new Properties("Monoid") {
      property("rightIdentity") = Prop.forAll { a: A =>
        A.append(a, A.empty) == a
      }
      property("leftIdentity") = Prop.forAll { a: A =>
        A.append(A.empty, a) == a
      }
      property("associativity") = Prop.forAll { (a: A, b: A, c: A) =>
        A.append(A.append(a, b), c) == A.append(a, A.append(b, c))
      }
    }

}
