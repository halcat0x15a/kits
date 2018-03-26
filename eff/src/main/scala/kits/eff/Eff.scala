package kits.eff

import scala.annotation.tailrec

sealed abstract class Eff[-R, +A] extends Product with Serializable {
  def map[B](f: A => B): Eff[R, B]

  def flatMap[S, B](f: A => Eff[S, B]): Eff[R with S, B]
}

object Eff {
  def apply[F: Manifest, A](fa: F): Eff[~[F], A] = Impure(Union(fa), Arrs((a: A) => Pure(a)))

  @tailrec
  def run[A](eff: Eff[Any, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a
      case Impure(Union.Tagless(a), k) => run[A](k(a))
    }

  def handleRelay[F, R, A, B](eff: Eff[~[F] with R, A])(pure: A => Eff[R, B])(bind: F => (Any => Eff[R, B]) => Eff[R, B])(implicit F: Manifest[F]): Eff[R, B] = {
    def go(eff: Eff[~[F] with R, A]): Eff[R, B] =
      eff match {
        case Pure(a) => pure(a)
        case Impure(Union.Tagless(a), k) => go(k(a))
        case Impure(Union.Tagged(`F`, fa: F), k) => bind(fa)(a => Impure(Union.Tagless(a), Arrs((a: A) => go(k(a)))))
        case Impure(r: Union[R], k) => Impure(r, Arrs((a: A) => go(k(a))))
      }
    go(eff)
  }

  def handleRelayS[F, R, S, A, B](s: S, eff: Eff[~[F] with R, A])(pure: (S, A) => Eff[R, B])(bind: F => (S, (S, Any) => Eff[R, B]) => Eff[R, B])(implicit F: Manifest[F]): Eff[R, B] = {
    def go(s: S, eff: Eff[~[F] with R, A]): Eff[R, B] =
      eff match {
        case Pure(a) => pure(s, a)
        case Impure(Union.Tagless(a), k) => go(s, k(a))
        case Impure(Union.Tagged(`F`, fa: F), k) => bind(fa)(s, (s, a) => Impure(Union.Tagless(a), Arrs((a: A) => go(s, k(a)))))
        case Impure(r: Union[R], k) => Impure(r, Arrs((a: A) => go(s, k(a))))
      }
    go(s, eff)
  }

  case class Pure[A](value: A) extends Eff[Any, A] {
    def map[B](f: A => B): Eff[Any, B] = Pure(f(value))
    def flatMap[S, B](f: A => Eff[S, B]): Eff[S, B] = f(value)
  }

  case class Impure[R, A, B](union: Union[R], arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Impure(union, arrs :+ (x => Pure(f(x))))
    def flatMap[S, C](f: B => Eff[S, C]): Eff[R with S, C] = Impure(union, arrs :+ f)
  }
}
