package kits

package spec

import org.scalacheck.Properties

object ConjSpec extends Properties("Conj") {

  include(MonoidSpec[Conj])

}
