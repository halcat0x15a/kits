package kits

package spec

import org.scalacheck.Properties

object ProdSpec extends Properties("Prod") {

  include(MonoidSpec[Prod[Int]])

}
