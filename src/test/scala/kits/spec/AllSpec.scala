package kits

package spec

import org.scalacheck.Properties

object AllSpec extends Properties("All") {

  include(MonoidSpec[Monoid.All])

}
