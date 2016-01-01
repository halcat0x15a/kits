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

sealed abstract case class Impure[U <: Union, A]() extends Free[U, A] { self =>

  type T

  def union: U { type T = self.T }

  def arrows: Queue[U, T, A]

  def map[B](f: A => B): Free[U, B] = Impure(union, arrows :+ (a => Pure(f(a))))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = Impure(union, arrows :+ f)

}

object Impure {

  def apply[U <: Union, A, B](u: U { type T = A }, a: Queue[U, A, B]): Impure[U, B] { type T = A } =
    new Impure[U, B] {
      type T = A
      val union: U { type T = A } = u
      val arrows: Queue[U, A, B] = a
    }

}

object Free {

  def run[A](free: Free[Void, A]): A =
    free match {
      case Pure(a) => a
    }

}
