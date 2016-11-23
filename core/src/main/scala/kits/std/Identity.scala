package kits

package std

trait IdentityFunctor extends Functor[Identity] {

  override final def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = f(fa)

}

trait IdentityMonad extends Monad[Identity] { self: IdentityFunctor =>

  override final def pure[A](a: A): Identity[A] = a

  override final def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B] = f(fa)

  override final def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

}

trait IdentityTraverse extends Traverse[Identity] { self: IdentityFunctor =>

  override final def traverse[F[_]: Applicative, A, B](fa: Identity[A])(f: A => F[B]): F[Identity[B]] = f(fa)

}
