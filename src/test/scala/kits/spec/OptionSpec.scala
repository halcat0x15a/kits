package kits

package spec

import org.scalacheck.Properties

object OptionSpec extends Properties("Option") {

  include(MonoidSpec[Option[String]])

  include(FunctorSpec[Option, AnyVal])

  include(ApplicativeSpec[Option, AnyVal])

  include(MonadSpec[Option, AnyVal])

  include(TraverseSpec[Option, Option, AnyVal])

}
