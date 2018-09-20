package kits.eff

import scala.annotation.tailrec

sealed abstract class Writer[W] extends Product with Serializable

object Writer {
  def apply[W](implicit W: Manifest[W]): Ops[W] = new Ops(W)

  def tell[W](w: W)(implicit W: Manifest[W]): Eff[Writer[W], Unit] = Eff(Tell(W, w))

  def fold[W, R, A, B](eff: Eff[Writer[W] with R, A])(z: B)(f: (B, W) => B)(implicit W: Manifest[W]): Eff[R, (B, A)] = {
    def go(eff: Eff[Writer[W] with R, A], acc: B): Eff[R, (B, A)] = loop(eff, acc)
    @tailrec
    def loop(eff: Eff[Writer[W] with R, A], acc: B): Eff[R, (B, A)] =
      eff match {
        case Eff.Pure(a) => Eff.Pure((acc, a))
        case Eff.Impure(Tell(W, w: W), k) => loop(k(()), f(acc, w))
        case Eff.Impure(r, k) => Eff.Impure(r.asInstanceOf[R], Arrs((a: Any) => go(k(a), acc)))
      }
    loop(eff, z)
  }

  def run[W: Manifest, R, A](eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] =
    fold[W, R, A, Vector[W]](eff)(Vector.empty[W])(_ :+ _)

  case class Tell[W](manifest: Manifest[_], value: W) extends Writer[W]

  class Ops[W](val manifest: Manifest[W]) extends AnyVal {
    def run[R, A](eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] = Writer.run[W, R, A](eff)(manifest)
  }
}
