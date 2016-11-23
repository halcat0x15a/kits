package kits

package std

trait EitherFunctor[E] extends Functor[({ type F[A] = Either[E, A] })#F] {

  override final def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.right.map(f)

}

trait EitherApplicative[E] extends Applicative[({ type F[A] = Either[E, A] })#F] { self: EitherFunctor[E] =>

  def monoid: Monoid[E]

  override final def pure[A](a: A): Either[E, A] = Right(a)

  override final def ap[A, B](fa: Either[E, A])(f: Either[E, A => B]): Either[E, B] =
    (f, fa) match {
      case (Right(f), Right(a)) => Right(f(a))
      case (Left(x), Left(y)) => Left(monoid.append(x, y))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }

}

trait EitherMonad[E] extends Monad[({ type F[A] = Either[E, A] })#F] { self: EitherFunctor[E] =>

  override final def pure[A](a: A): Either[E, A] = Right(a)

  override final def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.right.flatMap(f)

}

trait EitherTraverse[E] extends Traverse[({ type F[A] = Either[E, A] })#F] { self: EitherFunctor[E] =>

  override final def traverse[F[_], A, B](fa: Either[E, A])(f: A => F[B])(implicit F: Applicative[F]): F[Either[E, B]] =
    fa.fold(e => F.pure(Left(e)), a => F.map(f(a))(Right(_)))

}
