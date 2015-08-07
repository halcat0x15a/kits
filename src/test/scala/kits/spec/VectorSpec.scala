package kits

package spec

import org.scalacheck.Properties

object VectorSpec extends Properties("Vector") {

  include(MonoidSpec[Vector[AnyVal]])

  include(FunctorSpec[Vector, AnyVal])

  include(ApplicativeSpec[Vector, AnyVal])

  include(MonadSpec[Vector, AnyVal])

  include(TraverseSpec[Vector, Option, AnyVal])

}
