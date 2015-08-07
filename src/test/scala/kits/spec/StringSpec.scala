package kits

package spec

import org.scalacheck.Properties

class StringSpec extends Properties("String") {

  include(MonoidSpec[String])

}
