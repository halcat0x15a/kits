package kits.free

sealed abstract class Reader[R] { type T }

object Reader {

  case class Ask[R]() extends Reader[R] { type T = R }

  def run[U <: Union, R, A](free: Free[Reader[R] :+: U, A], value: R): Free[U, A] =
    Free.fold(free)(a => Pure(a)) {
      case Ask() => k => k(value)
    }

  def ask[U <: Union, R](implicit F: Member[Reader[R], U]): Free[U, R] = Free(F.inject(Ask()))

  def local[U <: Union, R, A](free: Free[U, A])(f: R => R)(implicit F: Member[Reader[R], U]): Free[U, A] =
    ask.flatMap { r0 =>
      val r = f(r0)
      Free.intercept(free)(a => Pure(a))((_: Reader[R]) match {
        case Ask() => k => k(r)
      })
    }

}
