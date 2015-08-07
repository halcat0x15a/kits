package kits

package spec

import org.scalacheck.Properties

object AnySpec extends Properties("Any") {

  include(MonoidSpec[Monoid.Any])

}
