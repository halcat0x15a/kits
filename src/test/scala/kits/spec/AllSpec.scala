package kits

package spec

import org.scalacheck.Arbitrary

class AllSpec extends MonoidSpec {

  type T = Monoid.All

  val monoid: Monoid[Monoid.All] = implicitly

  val arbT: Arbitrary[Monoid.All] = implicitly

}
