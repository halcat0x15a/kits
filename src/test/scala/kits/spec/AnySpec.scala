package kits

package spec

import org.scalacheck.Arbitrary

class AnySpec extends MonoidSpec {

  type T = Monoid.Any

  val monoid: Monoid[Monoid.Any] = implicitly

  val arbT: Arbitrary[Monoid.Any] = implicitly

}
