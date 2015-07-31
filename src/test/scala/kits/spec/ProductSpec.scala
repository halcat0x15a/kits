package kits

package spec

import org.scalacheck.Properties

object ProductSpec extends Properties("Product") {

  include(MonoidSpec[Product[Int]])

}