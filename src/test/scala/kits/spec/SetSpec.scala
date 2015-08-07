package kits

package spec

import org.scalacheck.Properties

object SetSpec extends Properties("Set") {

  include(MonoidSpec[Set[String]])

  include(FunctorSpec[Set, AnyVal])

  include(ApplicativeSpec[Set, AnyVal])

  include(MonadSpec[Set, AnyVal])

  include(TraverseSpec[Set, Option, AnyVal])

}
