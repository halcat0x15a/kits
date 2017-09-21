package kits.free

sealed abstract class Reader[R] {

  type Member[U] = kits.free.Member[Reader[R], U]

}

object Reader {

  case class Ask[R]() extends Reader[R]

  def run[U, R, A](free: Free[Reader[R] :+: U, A], value: R): Free[U, A] =
    Free.handleRelay(free)(
      a => Right(a),
      (_, k) => Left(k(value))
    )

  def askF[U, R](implicit F: Member[Reader[R], U]): Free[U, R] = Free(F.inject(Ask()))

  def localF[U: Reader[R]#Member, R, A](free: Free[U, A])(f: R => R): Free[U, A] =
    askF.flatMap { r0 =>
      val r = f(r0)
      Free.interpose(free)(
        a => Right(a),
        (_: Reader[R], k) => Left(k(r))
      )
    }

  def ask[R]: Eff[EffLeaf[Reader[R]], R] = Eff.impure(Ask())

  def local[Effs, R, A](eff: Eff[Effs, A])(f: R => R)(implicit get: GetMember[Reader[R], Effs]): Eff[Effs, A] =
    new Eff[Effs, A] {
      def free[U](implicit effs: Effs { type Union = U }): Free[U, A] = localF(eff.free[U])(f)(get(effs))
    }

  def handle[R](value: R) = new Handler {
    type Cons[U] = Reader[R] :+: U
    type Result[A] = A
    def apply[U, A](free: Free[Reader[R] :+: U, A]): Free[U, A] = Reader.run(free, value)
  }

}
