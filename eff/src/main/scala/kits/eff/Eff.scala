package kits.eff

import scala.reflect.Manifest

sealed abstract class Eff[-R, A] extends Product with Serializable {
  def map[B](f: A => B): Eff[R, B]

  def flatMap[S, B](f: A => Eff[S, B]): Eff[R with S, B]
}

object Eff {
  def apply[F: Manifest, A](fa: F): Eff[~[F], A] = Impure(Union(fa), Arrs((a: A) => Pure(a)))

  def run[A](eff: Eff[Any, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a
    }

  def handleRelay[F: Manifest, R, A, B](eff: Eff[~[F] with R, A])(pure: A => Eff[R, B])(bind: F => (Any => Eff[R, B]) => Eff[R, B]): Eff[R, B] =
    eff match {
      case Pure(a) => pure(a)
      case Impure(u, k) =>
        u.decomp[F, R] match {
          case Left(fa) => bind(fa)(a => handleRelay[F, R, A, B](k(a))(pure)(bind))
          case Right(r) => Impure(r, Arrs((a: A) => handleRelay[F, R, A, B](k(a))(pure)(bind)))
        }
    }

  case class Pure[R, A](value: A) extends Eff[R, A] {
    def map[B](f: A => B): Eff[R, B] = Pure(f(value))
    def flatMap[S, B](f: A => Eff[S, B]): Eff[R with S, B] = f(value)
  }

  case class Impure[R, A, B](union: Union[R], arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Impure(union, arrs :+ (x => Pure(f(x))))
    def flatMap[S, C](f: B => Eff[S, C]): Eff[R with S, C] = Impure(union, arrs :+ f)
  }
}
