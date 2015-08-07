package kits

package spec

import org.scalacheck.Properties

object MapSpec extends Properties("Map") {

  include(MonoidSpec[Map[AnyVal, String]])

  include(FunctorSpec[({ type F[A] = Map[AnyVal, A] })#F, AnyVal])

  include(TraverseSpec[({ type F[A] = Map[AnyVal, A] })#F, Option, AnyVal])

}
