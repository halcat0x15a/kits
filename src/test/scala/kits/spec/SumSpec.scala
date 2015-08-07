package kits

package spec

import org.scalacheck.Arbitrary

class SumSpec extends MonoidSpec {

  type T = Monoid.Sum[Int]

  val monoid: Monoid[Monoid.Sum[Int]] = implicitly

  val arbT: Arbitrary[Monoid.Sum[Int]] = implicitly

}
