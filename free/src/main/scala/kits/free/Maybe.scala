package kits.free

object Maybe {

  def run[U, A](free: Free[Maybe :+: U, A]): Free[U, Option[A]] = Error.run(free).map(_.fold(_ => None, Some(_)))

  def nothing[U: Maybe#Member, A]: Free[U, A] = Error.fail(())

  def fromOption[U: Maybe#Member, A](option: Option[A]): Free[U, A] = option.fold(nothing[U, A])(a => Pure(a))

  def orElse[U: Maybe#Member, A](x: Free[U, A], y: Free[U, A]): Free[U, A] = Error.recover(x)((_: Unit) => y)

  def handle[E] = new Handler {
    type Cons[U] = Maybe :+: U
    type Result[A] = Option[A]
    def apply[U, A](free: Free[Maybe :+: U, A]): Free[U, Option[A]] = Maybe.run(free)
  }

}
