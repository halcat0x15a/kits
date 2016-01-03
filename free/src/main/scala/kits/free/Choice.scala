package kits

package free

import scala.annotation.tailrec

sealed abstract class Choice[+A]

object Choice {

  case object Zero extends Choice[Nothing]

  case object Plus extends Choice[Boolean]

  def run[U <: Union, M[_], A](free: Free[Choice :+: U, A])(implicit M: MonadPlus[M]): Free[U, M[A]] = {
    def go(free: Free[Choice :+: U, A], stack: List[Free[Choice :+: U, A]], acc: M[A]): Free[U, M[A]] = loop(free, stack, acc)
    @tailrec
    def loop(free: Free[Choice :+: U, A], stack: List[Free[Choice :+: U, A]], acc: M[A]): Free[U, M[A]] =
      free match {
        case Pure(a) =>
          val r = M.plus(acc, M.pure(a))
          stack match {
            case Nil => Pure(r)
            case h :: t => loop(h, t, r)
          }
        case Impure(Inl(Zero), _) =>
          stack match {
            case Nil => Pure(acc)
            case h :: t => loop(h, t, acc)
          }
        case Impure(Inl(Plus), k) =>
          loop(k(true), k(false) :: stack, acc)
        case Impure(Inr(u), k) =>
          Impure(u, Arrows.singleton((x: Any) => go(k(x), stack, acc)))
      }
    loop(free, Nil, M.zero)
  }

  def zero[U <: Union](implicit F: Member[Choice, U]): Free[U, Nothing] = Free(Zero)

  def plus[U <: Union, A](x: Free[U, A], y: Free[U, A])(implicit F: Member[Choice, U]): Free[U, A] =
    Free(Plus).flatMap(test => if (test) x else y)

  implicit def monadPlus[U <: Union](implicit F: Member[Choice, U]): MonadPlus[({ type F[A] = Free[U, A] })#F] =
    new MonadPlus[({ type F[A] = Free[U, A] })#F] {
      def zero[A]: Free[U, A] = Choice.zero
      def pure[A](a: A): Free[U, A] = Pure(a)
      def plus[A](x: Free[U, A], y: Free[U, A]): Free[U, A] = Choice.plus(x, y)
      override def map[A, B](fa: Free[U, A])(f: A => B): Free[U, B] = fa.map(f)
      def flatMap[A, B](fa: Free[U, A])(f: A => Free[U, B]): Free[U, B] = fa.flatMap(f)
    }

}
