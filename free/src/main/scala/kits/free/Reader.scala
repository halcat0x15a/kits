package kits.free

sealed abstract class Reader[R] {

  type T

  type Member[U <: Union] = kits.free.Member[Reader[R], U]

}

object Reader {

  case class Ask[R]() extends Reader[R] { type T = R }

  def run[U <: Union, R, A](value: R)(free: Free[Reader[R] :+: U, A]): Free[U, A] =
    Free.handleRelay(free)(a => Pure(a)) {
      case Ask() => k => k(value)
    }

  def ask[U <: Union, R](implicit F: Member[Reader[R], U]): Free[U, R] = Free(F.inject(Ask()))

  def local[U <: Union: Reader[R]#Member, R, A](free: Free[U, A])(f: R => R): Free[U, A] =
    ask.flatMap { r0 =>
      val r = f(r0)
      Free.interpose(free)(a => Pure(a))((_: Reader[R]) match {
        case Ask() => k => k(r)
      })
    }

}
