package kits

package spec

import org.scalacheck.Properties

object FirstSpec extends Properties("First") {

  include(MonoidSpec[First[AnyVal]])

}
