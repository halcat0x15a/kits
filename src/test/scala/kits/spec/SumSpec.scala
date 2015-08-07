package kits

package spec

import org.scalacheck.Properties

object SumSpec extends Properties("Sum") {

  include(MonoidSpec[Monoid.Sum[Int]])

}
