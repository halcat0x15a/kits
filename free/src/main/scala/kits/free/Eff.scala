package kits.free

trait Eff[Effs, A] { self =>

  def free[U](implicit effs: Effs { type Union = U }): Free[U, A]

  def map[B](f: A => B): Eff[Effs, B] =
    new Eff[Effs, B] {
      def free[U](implicit effs: Effs { type Union = U }): Free[U, B] = self.free[U].map(f)
    }

  def flatMap[B, Effs0](f: A => Eff[Effs0, B]): Eff[Effs :*: Effs0, B] =
    new Eff[Effs :*: Effs0, B] {
      def free[U](implicit effs: (Effs :*: Effs0) { type Union = U }): Free[U, B] =
        self.free(effs.left).flatMap(a => f(a).free(effs.right))
    }

}

object Eff {

  def pure[A](a: A): Eff[EffEmpty, A] =
    new Eff[EffEmpty, A] {
      def free[U](implicit effs: EffEmpty { type Union = U }): Free[U, A] = Pure(a)
    }

  def impure[F, A](fa: F): Eff[EffLeaf[F], A] =
    new Eff[EffLeaf[F], A] {
      def free[U](implicit effs: EffLeaf[F] { type Union = U }): Free[U, A] = Free(effs.member.inject(fa))
    }

}

object Test extends App {
  def ask[R]: Eff[EffLeaf[Reader[R]], R] = Eff.impure(kits.free.Reader.Ask())

  Reader.handle(0).run((for {
    n <- ask[Int]
  } yield n).free)
}
