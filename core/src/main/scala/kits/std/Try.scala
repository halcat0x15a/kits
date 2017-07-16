package kits

package std

import scala.util.{Failure, Try, Success}

trait TryFunctor extends Functor[Try] {

  override final def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)

}

trait TryMonad extends Monad[Try] { self: TryFunctor =>

  override final def pure[A](a: A): Try[A] = Success(a)

  override final def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

}

trait TryTraverse extends Traverse[Try] { self: TryFunctor =>

  override final def traverse[F[_], A, B](fa: Try[A])(f: A => F[B])(implicit F: Applicative[F]): F[Try[B]] =
    fa match {
      case Success(a) => F.map(f(a))(Success(_))
      case Failure(e) => F.pure(Failure(e))
    }

}
