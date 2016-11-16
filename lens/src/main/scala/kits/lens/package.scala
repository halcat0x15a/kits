package kits

package object lens {

  implicit class WithLens[A](val a: A) extends AnyVal {
    def withLens(f: Lens[A, A] => A => A): A = f(Lens[A])(a)
  }

  implicit class AsOptional[A, B](val lens: Lens[A, Option[B]]) extends AnyVal {
    def asOpt: Optional[A, B] =
      new Optional[A, B] {
        def get(a: A): Option[B] = lens.get(a)
        def set(b: B)(a: A): A = lens.set(Some(b))(a)
      }
  }

}
