package kits

package std

trait OptionFunctor extends Functor[Option] {

  override final def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

}

trait OptionMonadPlus extends MonadPlus[Option] { self: OptionFunctor =>

  override final def zero[A]: Option[A] = None

  override final def pure[A](a: A): Option[A] = Some(a)

  override final def plus[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)

  override final def filter[A](fa: Option[A])(p: A => Boolean): Option[A] = fa.filter(p)

  override final def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

  override final def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten

}

trait OptionTraverse extends Traverse[Option] { self: OptionFunctor =>

  override final def traverse[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]): F[Option[B]] =
    fa.fold(F.pure(Option.empty[B]))(a => F.map(f(a))(Some(_)))

}

trait OptionMonoid[A] extends Monoid[Option[A]] {

  def monoid: Monoid[A]

  override final def empty: Option[A] = None

  override final def append(x: Option[A], y: Option[A]): Option[A] =
    (x, y) match {
      case (None, None) => None
      case (_, None) => x
      case (None, _) => y
      case (Some(a), Some(b)) => Some(monoid.append(a, b))
    }

}

trait FirstMonoid[A] extends Monoid[Option[A]] {

  override final def empty: Option[A] = None

  override final def append(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)

}

trait LastMonoid[A] extends Monoid[Option[A]] {

  override final def empty: Option[A] = None

  override final def append(x: Option[A], y: Option[A]): Option[A] = y.orElse(x)

}
