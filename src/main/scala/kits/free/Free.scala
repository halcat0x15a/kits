package kits.free

import scala.annotation.tailrec

sealed abstract class Free[U <: Union, +A] {

  def map[B](f: A => B): Free[U, B]

  def flatMap[B](f: A => Free[U, B]): Free[U, B]

}

case class Pure[U <: Union, A](value: A) extends Free[U, A] {

  def map[B](f: A => B): Free[U, B] = Pure(f(value))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = f(value)

}

sealed abstract case class Impure[U <: Union, A]() extends Free[U, A] {

  type T

  def union: U

  def arrows: Queue[U, T, A]

  def map[B](f: A => B): Free[U, B] = Impure(union, arrows :+ (a => Pure(f(a))))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = Impure(union, arrows :+ f)

}

object Impure {

  def apply[U <: Union, A, B](u: U, a: Queue[U, A, B]): Impure[U, B] { type T = A } =
    new Impure[U, B] {
      type T = A
      val union: U = u
      val arrows: Queue[U, A, B] = a
    }

}

object ImpureL {

  def unapply[F[_], U <: Union, A, B](free: Free[F :+: U, A]): Option[(F[A], Queue[F :+: U, A, B])] =
    free match {
      case Pure(_) => None
      case impure@Impure() =>
        impure.union match {
          case inl@Inl() => Some((inl.head.asInstanceOf[F[A]], impure.arrows.asInstanceOf[Queue[F :+: U, A, B]]))
          case Inr(_) => None
        }
    }

}

object ImpureR {

  def unapply[F[_], U <: Union, A, B](free: Free[F :+: U, A]): Option[(U, Queue[F :+: U, A, B])] =
    free match {
      case Pure(_) => None
      case impure@Impure() =>
        impure.union match {
          case Inl() => None
          case Inr(u) => Some((u, impure.arrows.asInstanceOf[Queue[F :+: U, A, B]]))
        }
    }

}

object Free {

  def run[A](free: Free[Void, A]): A =
    free match {
      case Pure(a) => a
    }

}
