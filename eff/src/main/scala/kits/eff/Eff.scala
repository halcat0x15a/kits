package kits.eff

import scala.reflect.runtime.universe.TypeTag

trait Fx[+A] extends Any

case class Union[R, A](tag: TypeTag[_], value: R with Fx[A])

sealed abstract class Eff[-R, +A] extends Product with Serializable {
  def map[B](f: A => B): Eff[R, B]

  def flatMap[S, B](f: A => Eff[S, B]): Eff[R with S, B]

  def withFilter(p: A => Boolean): Eff[Opt with R, A] = flatMap(a => if (p(a)) Eff.Pure(a) else Opt.empty)
}

object Eff {
  def apply[F, A](fa: F with Fx[A])(implicit F: TypeTag[F]): Eff[F, A] = Impure(Union[F, A](F, fa), Arrs.Leaf((a: A) => Pure(a)))

  def run[A](eff: Eff[Any, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a
    }

  case class Pure[A](value: A) extends Eff[Any, A] {
    def map[B](f: A => B): Eff[Any, B] = Pure(f(value))
    def flatMap[S, B](f: A => Eff[S, B]): Eff[S, B] = f(value)
  }

  case class Impure[R, A, B](union: Union[R, A], arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Impure(union, arrs :+ (x => Pure(f(x))))
    def flatMap[S, C](f: B => Eff[S, C]): Eff[R with S, C] = Impure(union.asInstanceOf[Union[R with S, A]], arrs :+ f)
  }
}
