package kits

package spec

import org.scalacheck.Arbitrary

class StringSpec extends MonoidSpec {

  type T = String

  val monoid: Monoid[String] = implicitly

  val arbT: Arbitrary[String] = implicitly

}
