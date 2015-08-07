package kits

package spec

import org.scalacheck.Properties

object ValidationSpec extends Properties("Validation") {

  include(FunctorSpec[({ type F[A] = Applicative.Validation[AnyVal, A] })#F, AnyVal])

  include(ApplicativeSpec[({ type F[A] = Applicative.Validation[String, A] })#F, AnyVal])

  include(TraverseSpec[({ type F[A] = Applicative.Validation[AnyVal, A] })#F, Option, AnyVal])

}
