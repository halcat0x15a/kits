package kits.eff

trait Interpreter[F, R, A, B] {
  def pure(a: A): Eff[R, B]

  def flatMap[T](fa: F with Fx[T])(f: T => Eff[R, B]): Eff[R, B]

  final def apply(eff: Eff[F with R, A])(implicit F: Manifest[F]): Eff[R, B] =
    eff match {
      case Eff.Pure(a) =>
        pure(a)
      case Eff.Impure(Union(tag, fa), k) if tag <:< F =>
        flatMap(fa.asInstanceOf[F with Fx[Any]])(a => apply(k(a)))
      case Eff.Impure(r, k) =>
        Eff.Impure(r.asInstanceOf[Union[R, Any]], Arrs.Leaf((a: Any) => apply(k(a))))
    }
}

trait StateInterpreter[F, R, S, A, B] {
  def pure(s: S, a: A): Eff[R, B]

  def flatMap[T](s: S, fa: F with Fx[T])(f: (S, T) => Eff[R, B]): Eff[R, B]

  final def apply(s: S, eff: Eff[F with R, A])(implicit F: Manifest[F]): Eff[R, B] =
    eff match {
      case Eff.Pure(a) =>
        pure(s, a)
      case Eff.Impure(Union(tag, fa), k) if tag <:< F =>
        flatMap(s, fa.asInstanceOf[F with Fx[Any]])((s, a) => apply(s, k(a)))
      case Eff.Impure(r, k) =>
        Eff.Impure(r.asInstanceOf[Union[R, Any]], Arrs.Leaf((a: Any) => apply(s, k(a))))
    }
}
