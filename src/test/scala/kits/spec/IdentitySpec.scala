package kits

package spec

import org.scalacheck.Properties

object IdentitySpec extends Properties("Identity") {

  include(FunctorSpec[Identity, AnyVal])

  include(ApplicativeSpec[Identity, AnyVal])

  include(MonadSpec[Identity, AnyVal])

  include(TraverseSpec[Identity, Option, AnyVal])

}
