package kits.eff

sealed abstract class Writer[W] extends Product with Serializable

object Writer {
  def tell[W: Manifest](w: W): Eff[Writer[W], Unit] = Eff(Tell(w))

  def fold[W: Manifest, R, A, B](eff: Eff[Writer[W] with R, A])(z: B)(f: (B, W) => B): Eff[R, (B, A)] = {
    val handle = new StateRecurser[Writer[W], R, B, A, (B, A)] {
      def pure(s: B, a: A) = Eff.Pure((s, a))
      def tailRec[T](s: B, ft: Writer[W] with Fx[T]) =
        ft match {
          case Tell(w) => Left((f(s, w), ()))
        }
    }
    handle(z, eff)
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

  case class Tell[W](value: W) extends Writer[W] with Fx[Unit]

  def apply[W](implicit W: Manifest[W]): Ops[W] = new Ops(W)

  class Ops[W](val manifest: Manifest[W]) extends AnyVal {
    def run[R, A](eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] = Writer.run[W, R, A](eff)(manifest)
    def listen[R <: Writer[W], A](eff: Eff[R, A]): Eff[R, (Vector[W], A)] = Writer.listen[W, R, A](eff)(manifest)
  }
}
