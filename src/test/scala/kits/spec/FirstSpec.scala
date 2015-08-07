package kits

package spec

import org.scalacheck.Arbitrary

class FirstSpec extends MonoidSpec {

  type T = Monoid.First[AnyVal]

  val monoid: Monoid[Monoid.First[AnyVal]] = implicitly

  val arbT: Arbitrary[Monoid.First[AnyVal]] = implicitly

}
