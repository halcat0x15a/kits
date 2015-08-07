package kits

package spec

import org.scalacheck.Arbitrary

class UnitSpec extends MonoidSpec {

  type T = Unit

  val monoid: Monoid[Unit] = implicitly

  val arbT: Arbitrary[Unit] = implicitly

}
