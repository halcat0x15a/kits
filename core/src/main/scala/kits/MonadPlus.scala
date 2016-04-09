package kits

import scala.language.implicitConversions

trait MonadPlus[F[_]] extends Monad[F] { F =>

  def zero[A]: F[A]

  def plus[A](x: F[A], y: F[A]): F[A]

  def filter[A](fa: F[A])(p: A => Boolean): F[A] = flatMap(fa)(a => if (p(a)) pure(a) else zero)

  class MonadPlusOps[A](self: F[A]) extends MonadOps(self) {

    def plus(that: F[A]): F[A] = F.plus(self, that)

    def filter(p: A => Boolean): F[A] = F.filter(self)(p)

    def withFilter(p: A => Boolean): F[A] = F.filter(self)(p)

  }

}

object MonadPlus {

  implicit def Ops[A](self: A)(implicit A: Unify[MonadPlus, A]): MonadPlus[A.F]#MonadPlusOps[A.A] = new A.TC.MonadPlusOps[A.A](A(self))

}
