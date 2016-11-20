package kits

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success}
import scala.util.control.TailCalls._

trait Monad[F[_]] extends Applicative[F] { F =>

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

}

object Monad {

  def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)

  def flatten[F[_], A](ffa: F[F[A]])(implicit F: Monad[F]): F[A] = F.flatten(ffa)

  implicit def Either[E]: Monad[({ type F[A] = Either[E, A] })#F] = new Monad.EitherMonad[E] with Functor.EitherFunctor[E] {}

  trait IdentityMonad extends Monad[Identity] { self: Functor.IdentityFunctor =>
    override final def pure[A](a: A): Identity[A] = a
    override final def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B] = f(fa)
    override final def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
  }

  trait Function0Monad extends Monad[Function0] { self: Functor.Function0Functor =>
    override final def pure[A](a: A): Function0[A] = () => a
    override final def ap[A, B](fa: Function0[A])(f: Function0[A => B]): Function0[B] = () => f()(fa())
    override final def flatMap[A, B](fa: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(fa())()
  }

  trait EitherMonad[E] extends Monad[({ type F[A] = Either[E, A] })#F] { self: Functor.EitherFunctor[E] =>
    override final def pure[A](a: A): Either[E, A] = Right(a)
    override final def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.right.flatMap(f)
  }

  trait TryMonad extends Monad[Try] {
    override final def pure[A](a: A): Try[A] = Success(a)
    override final def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
    override final def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
  }

  trait FutureMonad extends Monad[Future] {
    implicit def executor: ExecutionContext
    override final def pure[A](a: A): Future[A] = Future.successful(a)
    override final def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    override final def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
  }

  trait TailRecMonad extends Monad[TailRec] {
    override final def pure[A](a: A): TailRec[A] = done(a)
    override final def map[A, B](fa: TailRec[A])(f: A => B): TailRec[B] = fa.map(f)
    override final def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = tailcall(fa.flatMap(f))    
  }

}
