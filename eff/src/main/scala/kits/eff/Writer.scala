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
        case Eff.Impure(r, k) => Eff.Impure(r.asInstanceOf[R], Arrs.Leaf((a: Any) => go(k(a), acc)))
      }
    loop(eff, z)
  }

  def run[W: Manifest, R, A](eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] =
    fold[W, R, A, Vector[W]](eff)(Vector.empty[W])(_ :+ _)

  def listen[W: Manifest, R <: Writer[W], A](eff: Eff[R, A]): Eff[R, (Vector[W], A)] =
    for {
      r <- run[W, R, A](eff)
      _ <- r._1.foldLeft(Eff.Pure(()): Eff[R, Unit]) { (eff, w) =>
        eff.flatMap(_ => tell(w))
      }
    } yield r

  case class Tell[W](manifest: Manifest[_], value: W) extends Writer[W]

  class Ops[W](val manifest: Manifest[W]) extends AnyVal {
    def run[R, A](eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] = Writer.run[W, R, A](eff)(manifest)
    def listen[R <: Writer[W], A](eff: Eff[R, A]): Eff[R, (Vector[W], A)] = Writer.listen[W, R, A](eff)(manifest)
  }
}
