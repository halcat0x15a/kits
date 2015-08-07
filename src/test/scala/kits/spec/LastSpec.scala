package kits

package spec

import org.scalacheck.Arbitrary

class LastSpec extends MonoidSpec {

  type T = Monoid.Last[AnyVal]

  val monoid: Monoid[Monoid.Last[AnyVal]] = implicitly

  val arbT: Arbitrary[Monoid.Last[AnyVal]] = implicitly

}
