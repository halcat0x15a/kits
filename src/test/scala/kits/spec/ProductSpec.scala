package kits

package spec

import org.scalacheck.Arbitrary

class ProductSpec extends MonoidSpec {

  type T = Monoid.Product[Int]

  val monoid: Monoid[Monoid.Product[Int]] = implicitly

  val arbT: Arbitrary[Monoid.Product[Int]] = implicitly

}
