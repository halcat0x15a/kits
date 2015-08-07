package kits

package spec

import org.scalacheck.Properties

object ValidationSpec extends Properties("Validation") {

  include(FunctorSpec[({ type F[A] = Validation[AnyVal, A] })#F, AnyVal])

  include(ApplicativeSpec[({ type F[A] = Validation[String, A] })#F, AnyVal])

  include(TraverseSpec[({ type F[A] = Validation[AnyVal, A] })#F, Option, AnyVal])

}
