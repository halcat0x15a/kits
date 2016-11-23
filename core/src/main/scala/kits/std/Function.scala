package kits

package std

trait Function0Functor extends Functor[Function0] {

  override final def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = () => f(fa())

}

trait Function0Traverse extends Traverse[Function0] { self: Function0Functor =>

  override final def traverse[F[_], A, B](fa: Function0[A])(f: A => F[B])(implicit F: Applicative[F]): F[Function0[B]] = F.map(f(fa()))(b => () => b)

}

trait Function0Monad extends Monad[Function0] { self: Function0Functor =>

  override final def pure[A](a: A): Function0[A] = () => a

  override final def ap[A, B](fa: Function0[A])(f: Function0[A => B]): Function0[B] = () => f()(fa())

  override final def flatMap[A, B](fa: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(fa())()

}
