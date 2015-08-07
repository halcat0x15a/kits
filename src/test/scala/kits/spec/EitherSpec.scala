package kits

package spec

import org.scalacheck.Properties

object EitherSpec extends Properties("Either") {

  include(FunctorSpec[({ type F[A] = Either[AnyVal, A] })#F, AnyVal])

  include(ApplicativeSpec[({ type F[A] = Either[AnyVal, A] })#F, AnyVal])

  include(MonadSpec[({ type F[A] = Either[AnyVal, A] })#F, AnyVal])

  include(TraverseSpec[({ type F[A] = Either[AnyVal, A] })#F, Option, AnyVal])

}
