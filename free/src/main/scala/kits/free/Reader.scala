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

  def ask[R] = new Eff[R] {
    type Effects = EffLeaf[Reader[R]]
    def free[U](implicit members: EffMember[Effects, U]): Free[U, R] = Free(members.get.inject(Ask()))
  }

  def local[Effs <: EffTree, R, A](eff: Eff[A] { type Effects = Effs })(f: R => R)(implicit get: GetMember[Effs, Reader[R]]): Eff[A] {
    type Effects = EffLeaf[Reader[R]] :*: Effs
  } =
    ask[R].flatMap { r0 =>
      val r = f(r0)
      new Eff[A] {
        type Effects = Effs
        def free[U](implicit members: EffMember[Effects, U]): Free[U, A] =
          Free.interpose(eff.free[U])(
            a => Right(a),
            (_: Reader[R], k) => Left(k(r))
          )(get(members))
      }
    }

  def handle[R](value: R) = new Handler {
    type Cons[U] = Reader[R] :+: U
    type Result[A] = A
    def apply[U, A](free: Free[Reader[R] :+: U, A]): Free[U, A] = Reader.run(free, value)
  }

}
