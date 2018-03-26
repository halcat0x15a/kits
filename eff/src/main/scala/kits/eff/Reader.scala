package kits.eff

sealed abstract class Reader[I] extends Product with Serializable

object Reader {
  def ask[I: Manifest]: Eff[~[Reader[I]], I] = Eff(Ask[I]: Reader[I])

  def run[I: Manifest, R, A](i: I)(eff: Eff[~[Reader[I]] with R, A]): Eff[R, A] =
    Eff.handleRelay[Reader[I], R, A, A](eff)(a => Eff.Pure(a)) {
      case Ask() => k => k(i)
    }

  def local[I: Manifest, R <: ~[Reader[I]], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask[I].flatMap { r0 =>
      val r = f(r0)
      run[I, R, A](r)(eff)
    }

  case class Ask[I]() extends Reader[I]
}
