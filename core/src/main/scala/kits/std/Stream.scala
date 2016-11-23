package kits

package std

trait StreamFunctor extends Functor[Stream] {

  override final def map[A, B](fa: Stream[A])(f: A => B): Stream[B] = fa.map(f)

}

trait StreamMonadPlus extends MonadPlus[Stream] { self: StreamFunctor =>

  override final def zero[A]: Stream[A] = Stream.empty

  override final def pure[A](a: A): Stream[A] = Stream(a)

  override final def plus[A](x: Stream[A], y: Stream[A]): Stream[A] = x #::: y

  override final def filter[A](fa: Stream[A])(p: A => Boolean): Stream[A] = fa.filter(p)

  override final def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)

  override final def flatten[A](ffa: Stream[Stream[A]]): Stream[A] = ffa.flatten

}

trait StreamTraverse extends Traverse[Stream] { self: StreamFunctor =>

  override final def traverse[F[_], A, B](fa: Stream[A])(f: A => F[B])(implicit F: Applicative[F]): F[Stream[B]] =
    fa.foldRight(F.pure(Stream.empty[B]))((a, ga) => F.map2(f(a), ga)(_ #:: _))

}
