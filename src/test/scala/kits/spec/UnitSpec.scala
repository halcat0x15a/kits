package kits

package spec

import org.scalacheck.Properties

object UnitSpec extends Properties("Unit") {

  include(MonoidSpec[Unit])

}
