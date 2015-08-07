package kits

package spec

import org.scalacheck.Properties

object ListSpec extends Properties("List") {

  include(MonoidSpec[List[AnyVal]])

  include(FunctorSpec[List, AnyVal])

  include(ApplicativeSpec[List, AnyVal])

  include(MonadSpec[List, AnyVal])

  include(TraverseSpec[List, Option, AnyVal])

}
