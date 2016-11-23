package kits

package std

trait SetFunctor extends Functor[Set] {

  override final def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)

}

trait SetMonadPlus extends MonadPlus[Set] { self: SetFunctor =>

  override final def zero[A]: Set[A] = Set.empty

  override final def pure[A](a: A): Set[A] = Set(a)

  override final def plus[A](x: Set[A], y: Set[A]): Set[A] = x ++ y

  override final def filter[A](fa: Set[A])(p: A => Boolean): Set[A] = fa.filter(p)

  override final def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)

  override final def flatten[A](ffa: Set[Set[A]]): Set[A] = ffa.flatten

}

trait SetTraverse extends Traverse[Set] { self: SetFunctor =>

  override final def traverse[F[_], A, B](fa: Set[A])(f: A => F[B])(implicit F: Applicative[F]): F[Set[B]] =
    fa.foldLeft(F.pure(Set.empty[B]))((ga, a) => F.map2(ga, f(a))(_ + _))

}
