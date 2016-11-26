package kits.free

sealed abstract class Reader[R] {

  type Member[U] = kits.free.Member[Reader[R], U]

}

object Reader {

  case class Ask[R]() extends Reader[R]

  def run[R](value: R) = new Run {
    type Sum[U] = Reader[R] :+: U
    type F[A] = A
    def run[U, A](free: Free[Reader[R] :+: U, A]): Free[U, A] =
      Free.handleRelay(free)(a => a) {
        case Ask() => k => Left(k(value))
      }
  }

  def ask[U, R](implicit F: Member[Reader[R], U]): Free[U, R] = Free(F.inject(Ask()))

  def local[U: Reader[R]#Member, R, A](free: Free[U, A])(f: R => R): Free[U, A] =
    ask.flatMap { r0 =>
      val r = f(r0)
      Free.interpose(free)(a => a)((_: Reader[R]) match {
        case Ask() => k => Left(k(r))
      })
    }

}
