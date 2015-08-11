package kits

package spec

import org.scalacheck.Properties

object DisjSpec extends Properties("Disj") {

  include(MonoidSpec[Disj])

}
