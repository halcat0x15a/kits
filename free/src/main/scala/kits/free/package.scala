package kits

package object free {

  type Maybe[A] = Error[Unit, A]

  object Maybe {

    def run[U <: Union, A](free: Free[Maybe :+: U, A]): Free[U, Option[A]] = Error.run(free).map(_.right.toOption)

    def nothing[U <: Union](implicit F: Member[Maybe, U]): Free[U, Nothing] = Error.fail(())

  }

}
