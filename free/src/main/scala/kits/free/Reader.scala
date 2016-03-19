package kits.free

sealed abstract class Reader[R, +A]

object Reader {

  case class Ask[R]() extends Reader[R, R]

  def run[U <: Union, R, A](free: Free[({ type F[A] = Reader[R, A] })#F :+: U, A], value: R): Free[U, A] = {
    type F[A] = Reader[R, A]
    Free.fold(free: Free[F :+: U, A])(a => Pure(a)) {
      case Ask() => k => k(value)
    }
  }

  def ask[U <: Union, R](implicit reader: Member[({ type F[A] = Reader[R, A] })#F, U]): Free[U, R] = {
    type F[A] = Reader[R, A]
    Free(Ask(): F[R])
  }

  def local[U <: Union, R, A](free: Free[U, A])(f: R => R)(implicit reader: Member[({ type F[A] = Reader[R, A] })#F, U]): Free[U, A] = {
    type F[A] = Reader[R, A]
    ask flatMap { r0 =>
      val r = f(r0)
      Free.intercept(free)(a => Pure(a)) { (fa: F[Any]) =>
        fa match {
          case Ask() => k => k(r)
        }
      }
    }
  }

}
