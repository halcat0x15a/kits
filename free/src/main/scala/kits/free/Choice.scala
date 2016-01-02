package kits

package free

import scala.annotation.tailrec

sealed abstract class Choice[M[_], +A]

object Choice {

  case class Zero[M[_]]() extends Choice[M, Nothing]

  case class Plus[M[_]]() extends Choice[M, Boolean]

  def run[U <: Union, M[_], A](free: Free[({ type F[A] = Choice[M, A] })#F :+: U, A])(implicit M: MonadPlus[M]): Free[U, M[A]] = {
    type F[A] = Choice[M, A]
    @tailrec
    def go(free: Free[F :+: U, A], stack: List[Free[F :+: U, A]], acc: M[A]): Free[U, M[A]] =
      free match {
        case Pure(a) =>
          val r = M.plus(acc, M.pure(a))
          stack match {
            case Nil => Pure(r)
            case h :: t => go(h, t, r)
          }
        case Impure(Inl(Zero()), _) =>
          stack match {
            case Nil => Pure(M.zero)
            case h :: t => go(h, t, acc)
          }
        case Impure(Inl(Plus()), k) =>
          go(k(true), k(false) :: stack, acc)
        case Impure(Inr(u), k) =>
          Impure(u, Arrows.singleton((x: Any) => run(k(x))))
      }
    go(free, Nil, M.zero)
  }

  def zero[U <: Union, M[_]](implicit F: Member[({ type F[A] = Choice[M, A] })#F, U]): Free[U, Nothing] = {
    type F[A] = Choice[M, A]
    Free(Zero(): F[Nothing])
  }

  def plus[U <: Union, M[_], A](x: Free[U, A], y: Free[U, A])(implicit F: Member[({ type F[A] = Choice[M, A] })#F, U]): Free[U, A] = {
    type F[A] = Choice[M, A]
    Free(Plus(): F[Boolean]).flatMap(a => if (a) x else y)
  }

}
